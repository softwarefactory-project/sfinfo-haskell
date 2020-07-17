# zuul koji toolbox

## On koji, to enable sf-ci to create package:
Add to /etc/koji-hub/hub.conf, [policy] section
  package_list = all :: allow
Then restart httpd

## Prepare runtime for zuul_rpm_build.py
sudo yum install mock rpm-build rpmdevtools createrepo python-jinja2 python-semantic_version python-requests
sudo rsync -a rpm-gpg/ /etc/pki/rpm-gpg/
sudo usermod -a -G mock $USER
sudo mkdir /var/lib/sf
sudo ln -s /home/$SUDO_USER/zuul-rpm-build /var/lib/sf/zuul-rpm-build
sudo sed -i 's/\(%dist .el7\).*/\1/' /etc/rpm/macros.dist

## Release process

Check create-release.org from the www.softwarefactory-project.io repository.

## Cleanup zuul config and resources

./tools/get_package_lists.py sf-*.yaml | ./tools/clean_zuul_config.py ./zuul.d/*
./tools/get_package_lists.py sf-*.yaml | ./tools/clean_sf_resources.py ../config/resources/scl.yaml


## Using haskell stack to propose spec update:

Install sfinfo:

```ShellSession
$ sudo dnf copr enable -y petersen/stack2 && sudo dnf install -y stack && sudo stack upgrade
$ git clone https://softwarefactory-project.io/r/software-factory/gerrit-haskell ..
$ git clone https://softwarefactory-project.io/r/software-factory/podman-haskell ..
$ stack install sfinfo
$ sfinfo --help
SFInfo toolkit

Usage: sfinfo (compute-diff | propose-update)

Available options:
  -h,--help                Show this help text

Available commands:
  compute-diff             Compare package between rpm and pypi
  propose-update           Generate git reviews to bump outdated packages
```

Install pkgtreediff:

```ShellSession
$ git clone https://github.com/juhp/pkgtreediff
$ pushd pkgtreediff; stack install .; popd
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
$ sfinfo propose-update outdated-list.txt
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
