# software factory release toolbox

Install sfinfo:

```ShellSession
$ sudo dnf install -y cabal-install ghc
$ for dep in gerrit podman zuul; do git clone https://softwarefactory-project.io/r/software-factory/${dep}-haskell ../${dep}-haskell; done
$ cabal install exe:sfinfo
$ sfinfo --help
SFInfo toolkit

Usage: sfinfo (compute-diff | propose-update)

Available options:
  -h,--help                Show this help text

Available commands:
  compute-diff             Compare package between rpm and pypi
  propose-update           Generate git reviews to bump outdated packages
```

Compute outdated package list:

```ShellSession
$ sfinfo compute-diff outdated-list.txt
...
Outdated packages:
python3-APScheduler: 3.5.3-1.el7 -> 3.6.3-1.el7
python3-CacheControl: 0.12.5-1.el7 -> 0.12.6-1.el7
...
```

Propose distgit update in gerrit:

```ShellSession
$ export GERRIT_PASSWORD="XXXX"
$ sfinfo propose-update outdated-list.txt GerritUserName
...
Cloning into '/home/fedora/src/softwarefactory-project.io/rpms/python-kazoo'...
== submitting review with> "/home/fedora/src/softwarefactory-project.io/rpms/python-kazoo"
...
remote: Resolving deltas: 100% (1/1)
remote: Processing changes: new: 1, refs: 1, done
remote:
remote: New Changes:
remote:   https://softwarefactory-project.io/r/18879 Bump to 2.8.0
remote:
To ssh://softwarefactory-project.io:29418/rpms/python-kazoo
 * [new branch]      HEAD -> refs/for/master
```
