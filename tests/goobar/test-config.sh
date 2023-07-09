# Ensure the default configuration works.
goobar --one-shot

# Extract and test the README example.
tmp_dir="$(mktemp -d)"
trap "rm -rf $tmp_dir" EXIT
awk '/\(use-modules/{inc=1} /```/{inc=0} inc' README.md \
    > "$tmp_dir/config.scm"
goobar --one-shot -c "$tmp_dir/config.scm"
