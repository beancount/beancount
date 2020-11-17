"""ICU library for Unicode support."""

def setup_icu():
    # 2019-10-02
    http_archive(
        name = "icu",
        build_file = "//third_party/icu:icu.BUILD",
        strip_prefix = "icu",
        sha256 = "53e37466b3d6d6d01ead029e3567d873a43a5d1c668ed2278e253b683136d948",
        urls = ["https://github.com/unicode-org/icu/releases/download/release-65-1/icu4c-65_1-src.tgz"],
        patches = ["//third_party/icu:icu4c-64_2.patch"],
    )
