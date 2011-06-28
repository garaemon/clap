#!/usr/bin/env python
from argparse import ArgumentParser

# epilog
parser = ArgumentParser(description = "A foo that bars",
                        epilog = "And that's how you'd foo a bar")
parser.print_help()

# add-help sample
parser = ArgumentParser(prog = "PROG",
                        add_help = False)
parser.add_argument("--foo", help = "foo help")
parser.print_help()
                        
# prefix-chars usage
parser = ArgumentParser(prog = "PROG",
                        prefix_chars = "+")
parser.print_help()

parser = ArgumentParser(prog = "PROG",
                        prefix_chars = "+")
parser.add_argument("+f")
parser.add_argument("++bar")
print parser.parse_args("+f X ++bar Y".split())

# parent sample
parent_parser = ArgumentParser(add_help = False)
parent_parser.add_argument("--parent", type = int)
foo_parser = ArgumentParser(parents = [parent_parser])
foo_parser.add_argument("foo")
print foo_parser.parse_args("--parent 2 XXX".split())
bar_parser = ArgumentParser(parents = [parent_parser])
bar_parser.add_argument("--bar")
print bar_parser.parse_args("--parent 2 --bar YYY".split())
