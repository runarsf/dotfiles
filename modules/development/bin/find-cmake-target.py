import sys
import os
import json
import argparse

def find_cmake_target(source_file: str, project_root: str, build_dir: str) -> str|None:
    project_root = os.path.abspath(project_root)
    source_file = os.path.relpath(source_file, project_root)

    # Use absolute path for build_dir if it's relative
    if not os.path.isabs(build_dir):
        build_dir = os.path.join(project_root, build_dir)

    # Ensure CMake file API query exists
    query_dir = os.path.join(build_dir, ".cmake", "api", "v1", "query")
    os.makedirs(query_dir, exist_ok=True)

    # Create query file
    with open(os.path.join(query_dir, "codemodel-v2"), "w") as f:
        _ = f.write("")

    # Run CMake to generate file API response
    _ = os.system(f"cd {project_root} && cmake -B {build_dir} -S . >/dev/null 2>&1")

    reply_dir = os.path.join(build_dir, ".cmake", "api", "v1", "reply")

    if not os.path.exists(reply_dir):
        print("CMake file API query failed", file=sys.stderr)
        sys.exit(1)

    index_files = [f for f in os.listdir(reply_dir) if f.startswith("index-")]
    if not index_files:
        print("No index files found", file=sys.stderr)
        sys.exit(1)

    with open(os.path.join(reply_dir, index_files[0])) as f:
        index = json.load(f)

    codemodel_file = index["reply"]["codemodel-v2"]["jsonFile"]
    with open(os.path.join(reply_dir, codemodel_file)) as f:
        codemodel = json.load(f)

    for config in codemodel["configurations"]:
        for target_ref in config["targets"]:
            target_file = target_ref["jsonFile"]
            with open(os.path.join(reply_dir, target_file)) as f:
                target_data = json.load(f)

            if target_data.get("type") != "EXECUTABLE":
                continue

            if "sources" in target_data:
                for source in target_data["sources"]:
                    source_path = source.get("path", "")
                    if source_file == source_path:
                        return target_data["name"]

    return None

def main():
    parser = argparse.ArgumentParser(
        description="Find CMake target for a given source file",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    _ = parser.add_argument(
        "source_file",
        help="Source file to find the CMake target for"
    )
    _ = parser.add_argument(
        "project_root",
        help="Root directory of the CMake project"
    )
    _ = parser.add_argument(
        "-b", "--build",
        dest="build_dir",
        default="build",
        help="Build directory (default: build)"
    )

    args = parser.parse_args()

    if not (target := find_cmake_target(args.source_file, args.project_root, args.build_dir)):
        print(f"Target not found for {args.source_file} in {args.project_root}", file=sys.stderr)
        sys.exit(1)

    print(target)
    sys.exit(0)

if __name__ == "__main__":
    main()

