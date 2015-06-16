```
polypaver [OPTIONS] [PROBLEM_ID]
  Tries to decide numerical conjectures (problems) using polynomial enclosures.
  [PROBLEM_ID] specifies one or more conjectures as follows:
  <name>.pp [<conclusion number>]: like a single VC in SPARK .siv
  <name>.siv [<vc name> [<conclusion number>]]: SPARK-generated VCs
  <name>.tptp: TPTP file with fof formulas (ignoring includes)
  <name>.form: using internal syntax (machine generated)

Problem parameters:
  -i --tightnessvalues=ITEM  value(s) of T to try (if the formula has an
                             unbound variable T) (eg "2^0..10" or "1..10" or
                             "1,10,100") (default = 1)
Box solving effort:
  -s --startdegree=INT       first polynomial degree to try on each box
                             (default = degree)
  -d --degree=INT            maximum polynomial degree (default = 0)
  -z --maxsize=INT           maximum polynomial term size (default = 100)
  -e --effort=INT            for approximating point-wise sqrt and exp
                             (default = 10)
  -I --minintegrexp=INT      n to compute approximate integration step using
                             2^(-n)
Box subdivision strategy:
  -o --order=ORDER           sub-problem processing order, bfs for
                             breadth-first or dfs for depth-first, (default =
                             dfs)
  -f --splitintfirst         split integer valued domains until they are
                             exact before splitting the continuous domains
  -m --mindepth=INT          minimum bisection depth (default = 0)
  -b --maxdepth=INT          maximum bisection depth (default = 1000)
  -u --maxqueuelength=INT    maximum queue size (default = 50 for depth-first
                             and 5000 for breadth-first order)
  -t --time=INT              timeout in seconds (default = 7*24*3600 ie 1
                             week)                                                                                                                                          
Verbosity:
  -q --quiet                 no reporting of progress on the console (default
                             off)
  -v --verbose               report extra progress details on the console
                             (default off)
Plotting:
  -w --plotwidth=INT         plot width for 2D problems, 0 mean no plotting
                             (default)
  -h --plotheight=INT        plot height for 2D problems, 0 mean no plotting
                             (default)
  -? --help                  Display help message
  -V --version               Print version information
     --numeric-version       Print just the version number
```



