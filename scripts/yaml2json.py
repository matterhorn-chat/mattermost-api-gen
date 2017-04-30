#!/usr/bin/env python3

# Note, you may need to install pyyaml first.
# Something like: pip install pyyaml

import yaml, json, sys
sys.stdout.write(json.dumps(yaml.load(sys.stdin), sort_keys=True, indent=2))
