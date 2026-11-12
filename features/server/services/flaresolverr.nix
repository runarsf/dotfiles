{ self, lib', ... }:
{
  # The domain serves a proxy in front of flaresolverr, not flaresolverr
  # itself: it turns cooked.wiki pages into recipe JSON-LD.
  flake.nixosModules.flaresolverr = lib'.mkService {
    inherit self;

    name = "flaresolverr";
    subdomain = "cooked";
    port = 8192;

    module =
      { pkgs, ... }:
      let
        recipeProxyEnv = pkgs.python3.withPackages (
          ps: with ps; [
            fastapi
            uvicorn
            httpx
            beautifulsoup4
          ]
        );

        recipeProxyScript = pkgs.writeText "recipe-proxy.py" ''
        import json
        import httpx
        from bs4 import BeautifulSoup
        from fastapi import FastAPI, Request, Response

        app = FastAPI()
        FLARESOLVERR = "http://127.0.0.1:8191/v1"
        TARGET_BASE = "https://cooked.wiki"


        def build_recipe_jsonld(soup: BeautifulSoup) -> dict | None:
            root = soup.select_one('[itemtype="http://schema.org/Recipe"]')
            if not root:
                return None

            name_el = root.select_one('[itemprop="name"]')
            name = name_el.get_text(strip=True) if name_el else None

            yield_el = root.select_one('[itemprop="recipeYield"]')
            recipe_yield = yield_el.get_text(strip=True) if yield_el else None

            ingredients = []
            for li in root.select("ul.ingredients > div.shopping-list > li.ingredient[itemprop='recipeIngredient']"):
                checkbox = li.select_one("input[name='ingredients']")
                if checkbox and checkbox.get("value"):
                    ingredients.append(checkbox["value"])
                else:
                    ingredients.append(li.get_text(" ", strip=True))

            step_parts = []
            for li in root.select("ol.steps > li"):
                summary = li.select_one("span.summary")
                if summary:
                    desc = summary.find_next_sibling("span", class_="tagged-step")
                    title = summary.get_text(strip=True)
                    text = desc.get_text(" ", strip=True) if desc else ""
                    step_text = f"1. ### {title}:\n{text}" if text else f"1. ### {title}"
                else:
                    li_copy = li.__copy__()
                    for ul in li_copy.select("ul.narrative-ingredients"):
                        ul.decompose()
                    step_text = "1. " + li_copy.get_text(" ", strip=True)

                if step_text:
                    step_parts.append(step_text)

            og_image = soup.find("meta", property="og:image")
            image = og_image["content"] if og_image else None

            return {
                "@context": "https://schema.org",
                "@type": "Recipe",
                "name": name,
                "image": image,
                "recipeYield": recipe_yield,
                "recipeIngredient": ingredients,
                "recipeInstructions": "\n\n".join(step_parts),
            }


        def strip_microdata_attrs(soup: BeautifulSoup) -> None:
            for attr in ("itemprop", "itemtype", "itemscope"):
                for tag in soup.find_all(attrs={attr: True}):
                    del tag[attr]


        async def flaresolverr_get(url: str) -> str:
            payload = {"cmd": "request.get", "url": url, "maxTimeout": 60000}
            async with httpx.AsyncClient(timeout=70) as client:
                r = await client.post(FLARESOLVERR, json=payload)
            return r.json()["solution"]["response"]


        @app.get("/{path:path}")
        async def proxy(path: str, request: Request):
            target_url = f"{TARGET_BASE}/{path}"
            if request.url.query:
                target_url += f"?{request.url.query}"

            html = await flaresolverr_get(target_url)
            soup = BeautifulSoup(html, "html.parser")

            recipe = build_recipe_jsonld(soup)

            if recipe:
                strip_microdata_attrs(soup)
                script_tag = soup.new_tag("script", type="application/ld+json")
                script_tag.string = json.dumps(recipe)
                if soup.head:
                    soup.head.append(script_tag)
                else:
                    soup.insert(0, script_tag)

            return Response(content=str(soup), media_type="text/html")
        '';

        recipeProxySrc = pkgs.runCommand "recipe-proxy-src" { } ''
          mkdir -p $out
          cp ${recipeProxyScript} $out/main.py
        '';
      in
      {
        features.containers.units.flaresolverr = {
          image = "ghcr.io/flaresolverr/flaresolverr:latest";
          ports = [ "8191:8191" ];
          environment = {
            LOG_LEVEL = "info";
          };
        };

        systemd.services.recipe-proxy = {
          description = "FlareSolverr proxy for recipe imports";
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            ExecStart = "${recipeProxyEnv}/bin/uvicorn --app-dir ${recipeProxySrc} main:app --host 127.0.0.1 --port 8192";
            DynamicUser = true;
            Restart = "on-failure";
            RestartSec = 5;
          };
        };
      };
  };
}
