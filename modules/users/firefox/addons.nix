eat my ass
{ buildFirefoxXpiAddon, fetchurl, lib, stdenv }:
  {
    "adaptive-tab-bar-colour" = buildFirefoxXpiAddon {
      pname = "adaptive-tab-bar-colour";
      version = "2.1.4";
      addonId = "ATBC@EasonWong";
      url = "https://addons.mozilla.org/firefox/downloads/file/4159211/adaptive_tab_bar_colour-2.1.4.xpi";
      sha256 = "401e064048826dd627f02043ee04081b7caed191161f3de7cc89c22b4a222ff8";
      meta = with lib;
      {
        homepage = "https://github.com/easonwong-de/Adaptive-Tab-Bar-Colour";
        description = "Changes the color of Firefox tab bar to match the website theme.";
        license = licenses.mit;
        mozPermissions = [
          "activeTab"
          "tabs"
          "theme"
          "storage"
          "browserSettings"
          "management"
          "<all_urls>"
        ];
        platforms = platforms.all;
      };
    };
    "dont-track-me-google1" = buildFirefoxXpiAddon {
      pname = "dont-track-me-google1";
      version = "4.28";
      addonId = "dont-track-me-google@robwu.nl";
      url = "https://addons.mozilla.org/firefox/downloads/file/4132891/dont_track_me_google1-4.28.xpi";
      sha256 = "25bc90005d6f28d53182ef48c7efcbba72b198ce67cf1f0747dbd23d43078b26";
      meta = with lib;
      {
        homepage = "https://github.com/Rob--W/dont-track-me-google";
        description = "Removes the annoying link-conversion at Google Search / maps / ...";
        license = licenses.mit;
        mozPermissions = [
          "storage"
          "*://*.google.com/*"
          "*://*.google.ad/*"
          "*://*.google.ae/*"
          "*://*.google.com.af/*"
          "*://*.google.com.ag/*"
          "*://*.google.com.ai/*"
          "*://*.google.al/*"
          "*://*.google.am/*"
          "*://*.google.co.ao/*"
          "*://*.google.com.ar/*"
          "*://*.google.as/*"
          "*://*.google.at/*"
          "*://*.google.com.au/*"
          "*://*.google.az/*"
          "*://*.google.ba/*"
          "*://*.google.com.bd/*"
          "*://*.google.be/*"
          "*://*.google.bf/*"
          "*://*.google.bg/*"
          "*://*.google.com.bh/*"
          "*://*.google.bi/*"
          "*://*.google.bj/*"
          "*://*.google.com.bn/*"
          "*://*.google.com.bo/*"
          "*://*.google.com.br/*"
          "*://*.google.bs/*"
          "*://*.google.bt/*"
          "*://*.google.co.bw/*"
          "*://*.google.by/*"
          "*://*.google.com.bz/*"
          "*://*.google.ca/*"
          "*://*.google.cd/*"
          "*://*.google.cf/*"
          "*://*.google.cg/*"
          "*://*.google.ch/*"
          "*://*.google.ci/*"
          "*://*.google.co.ck/*"
          "*://*.google.cl/*"
          "*://*.google.cm/*"
          "*://*.google.cn/*"
          "*://*.google.com.co/*"
          "*://*.google.co.cr/*"
          "*://*.google.com.cu/*"
          "*://*.google.cv/*"
          "*://*.google.com.cy/*"
          "*://*.google.cz/*"
          "*://*.google.de/*"
          "*://*.google.dj/*"
          "*://*.google.dk/*"
          "*://*.google.dm/*"
          "*://*.google.com.do/*"
          "*://*.google.dz/*"
          "*://*.google.com.ec/*"
          "*://*.google.ee/*"
          "*://*.google.com.eg/*"
          "*://*.google.es/*"
          "*://*.google.com.et/*"
          "*://*.google.fi/*"
          "*://*.google.com.fj/*"
          "*://*.google.fm/*"
          "*://*.google.fr/*"
          "*://*.google.ga/*"
          "*://*.google.ge/*"
          "*://*.google.gg/*"
          "*://*.google.com.gh/*"
          "*://*.google.com.gi/*"
          "*://*.google.gl/*"
          "*://*.google.gm/*"
          "*://*.google.gp/*"
          "*://*.google.gr/*"
          "*://*.google.com.gt/*"
          "*://*.google.gy/*"
          "*://*.google.com.hk/*"
          "*://*.google.hn/*"
          "*://*.google.hr/*"
          "*://*.google.ht/*"
          "*://*.google.hu/*"
          "*://*.google.co.id/*"
          "*://*.google.ie/*"
          "*://*.google.co.il/*"
          "*://*.google.im/*"
          "*://*.google.co.in/*"
          "*://*.google.iq/*"
          "*://*.google.is/*"
          "*://*.google.it/*"
          "*://*.google.je/*"
          "*://*.google.com.jm/*"
          "*://*.google.jo/*"
          "*://*.google.co.jp/*"
          "*://*.google.co.ke/*"
          "*://*.google.com.kh/*"
          "*://*.google.ki/*"
          "*://*.google.kg/*"
          "*://*.google.co.kr/*"
          "*://*.google.com.kw/*"
          "*://*.google.kz/*"
          "*://*.google.la/*"
          "*://*.google.com.lb/*"
          "*://*.google.li/*"
          "*://*.google.lk/*"
          "*://*.google.co.ls/*"
          "*://*.google.lt/*"
          "*://*.google.lu/*"
          "*://*.google.lv/*"
          "*://*.google.com.ly/*"
          "*://*.google.co.ma/*"
          "*://*.google.md/*"
          "*://*.google.me/*"
          "*://*.google.mg/*"
          "*://*.google.mk/*"
          "*://*.google.ml/*"
          "*://*.google.com.mm/*"
          "*://*.google.mn/*"
          "*://*.google.ms/*"
          "*://*.google.com.mt/*"
          "*://*.google.mu/*"
          "*://*.google.mv/*"
          "*://*.google.mw/*"
          "*://*.google.com.mx/*"
          "*://*.google.com.my/*"
          "*://*.google.co.mz/*"
          "*://*.google.com.na/*"
          "*://*.google.com.nf/*"
          "*://*.google.com.ng/*"
          "*://*.google.com.ni/*"
          "*://*.google.ne/*"
          "*://*.google.nl/*"
          "*://*.google.no/*"
          "*://*.google.com.np/*"
          "*://*.google.nr/*"
          "*://*.google.nu/*"
          "*://*.google.co.nz/*"
          "*://*.google.com.om/*"
          "*://*.google.com.pa/*"
          "*://*.google.com.pe/*"
          "*://*.google.com.pg/*"
          "*://*.google.com.ph/*"
          "*://*.google.com.pk/*"
          "*://*.google.pl/*"
          "*://*.google.pn/*"
          "*://*.google.com.pr/*"
          "*://*.google.ps/*"
          "*://*.google.pt/*"
          "*://*.google.com.py/*"
          "*://*.google.com.qa/*"
          "*://*.google.ro/*"
          "*://*.google.ru/*"
          "*://*.google.rw/*"
          "*://*.google.com.sa/*"
          "*://*.google.com.sb/*"
          "*://*.google.sc/*"
          "*://*.google.se/*"
          "*://*.google.com.sg/*"
          "*://*.google.sh/*"
          "*://*.google.si/*"
          "*://*.google.sk/*"
          "*://*.google.com.sl/*"
          "*://*.google.sn/*"
          "*://*.google.so/*"
          "*://*.google.sm/*"
          "*://*.google.sr/*"
          "*://*.google.st/*"
          "*://*.google.com.sv/*"
          "*://*.google.td/*"
          "*://*.google.tg/*"
          "*://*.google.co.th/*"
          "*://*.google.com.tj/*"
          "*://*.google.tk/*"
          "*://*.google.tl/*"
          "*://*.google.tm/*"
          "*://*.google.tn/*"
          "*://*.google.to/*"
          "*://*.google.com.tr/*"
          "*://*.google.tt/*"
          "*://*.google.com.tw/*"
          "*://*.google.co.tz/*"
          "*://*.google.com.ua/*"
          "*://*.google.co.ug/*"
          "*://*.google.co.uk/*"
          "*://*.google.com.uy/*"
          "*://*.google.co.uz/*"
          "*://*.google.com.vc/*"
          "*://*.google.co.ve/*"
          "*://*.google.vg/*"
          "*://*.google.co.vi/*"
          "*://*.google.com.vn/*"
          "*://*.google.vu/*"
          "*://*.google.ws/*"
          "*://*.google.rs/*"
          "*://*.google.co.za/*"
          "*://*.google.co.zm/*"
          "*://*.google.co.zw/*"
          "*://*.google.cat/*"
          "*://*.google.ng/*"
        ];
        platforms = platforms.all;
      };
    };
    "sidetabs" = buildFirefoxXpiAddon {
      pname = "sidetabs";
      version = "0.63";
      addonId = "{ccc8cbaa-3c36-46d1-b0ae-d5e122755901}";
      url = "https://addons.mozilla.org/firefox/downloads/file/4232543/sidetabs-0.63.xpi";
      sha256 = "2c9baddd4932048697cb4f5883f2e60ae7f01f4c21eb27b71a5af9e449f66056";
      meta = with lib;
      {
        description = "Sidetabs will display your tabs in the sidebar. It's intended as a replacement for Firefox's default horizontal tabs. \nSidetabs is designed to look and feel just like Firefox tabs, only vertical. It has custom theme support and consistent styles.";
        license = licenses.gpl3;
        mozPermissions = [
          "tabs"
          "menus"
          "menus.overrideContext"
          "bookmarks"
          "sessions"
          "cookies"
          "contextualIdentities"
          "theme"
          "storage"
          "browsingData"
        ];
        platforms = platforms.all;
      };
    };
    "smart-https-revived" = buildFirefoxXpiAddon {
      pname = "smart-https-revived";
      version = "0.3.2";
      addonId = "{b3e677f4-1150-4387-8629-da738260a48e}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3975277/smart_https_revived-0.3.2.xpi";
      sha256 = "9e231d01f0474eac4643f39df9f5d57db8bfd350278b1edb4636bf56000ad124";
      meta = with lib;
      {
        homepage = "https://mybrowseraddon.com/smart-https.html";
        description = "This extension automatically changes HTTP web addresses to the secure HTTPS, whenever possible.";
        license = licenses.mpl20;
        mozPermissions = [
          "storage"
          "http://*/*"
          "https://*/*"
          "webRequest"
          "webRequestBlocking"
        ];
        platforms = platforms.all;
      };
    };
    "video-resumer" = buildFirefoxXpiAddon {
      pname = "video-resumer";
      version = "1.2.3";
      addonId = "videoresumer@jetpack";
      url = "https://addons.mozilla.org/firefox/downloads/file/726332/video_resumer-1.2.3.xpi";
      sha256 = "9f5e86529019d2edcc5707646f86f6018a1a34dd7243d38718f60bad59e76a74";
      meta = with lib;
      {
        description = "Automatically resumes YouTube videos from where you played them last. Without this extension, for example, when you click through YouTube videos, back and forth, they always start from the beginning.";
        mozPermissions = [
          "storage"
          "unlimitedStorage"
          "alarms"
          "*://*.youtube.com/*"
          "*://*.youtube.com/embed/*"
          "*://*.youtube-nocookie.com/embed/*"
        ];
        platforms = platforms.all;
      };
    };
  }