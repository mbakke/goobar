# Ensure i3bar output is valid JSON.
json="$(goobar -o i3bar --one-shot)"
jq -e . <<< "$json"
