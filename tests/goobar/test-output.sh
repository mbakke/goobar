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

# Ensure i3bar output is valid JSON.
json="$(goobar -o i3bar --one-shot)"
jq -e . <<< "$json"
