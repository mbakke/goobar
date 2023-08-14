# This file is part of Goobar.
#
# Copyright © 2023 Marius Bakke
#
# Goobar is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Goobar is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Goobar. If not, see <https://www.gnu.org/licenses/>.

tmp_dir="$(mktemp -d)"
trap "rm -rf $tmp_dir" EXIT

# Save the PID file to a temporary directory to avoid races.
export XDG_RUNTIME_DIR="$tmp_dir"

# Ensure the default configuration works.
goobar --one-shot

# Extract and test the README example.
awk '/\(use-modules/{inc=1} /```/{inc=0} inc' README.md \
    > "$tmp_dir/config.scm"
goobar --one-shot -c "$tmp_dir/config.scm"
