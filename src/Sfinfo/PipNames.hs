{-# LANGUAGE OverloadedStrings #-}

-- | An oracle table
module Sfinfo.PipNames
  ( ignoreList,
    pipList,
  )
where

import Data.Text (Text)

ignoreList :: [Text]
ignoreList =
  [ "pyasn1-modules" -- This is provided by pyasn1
  ]

pipList :: [(Text, Text)]
pipList =
  [ ("ansible", "ansible"),
    ("nodepool", "nodepool"),
    ("zuul", "zuul"),
    ("Babel", "python3-babel"),
    ("fb-re2", "python3-re2"),
    ("gitdb2", "python3-gitdb"),
    ("github3.py", "python3-github3"),
    ("Jinja2", "python3-jinja2"),
    ("Mako", "python3-mako"),
    ("MakupSafe", "python3-markupsafe"),
    ("msgpack-python", "python3-msgpack"),
    ("pycrypto", "python3-crypto"),
    ("PyJWT", "python3-jwt"),
    ("jsonpath-rw", "python3-jsonpath-rw"),
    ("ply", "python3-ply"),
    ("PyNaCl", "python3-pynacl"),
    ("PyYAML", "python3-pyyaml"),
    ("smmap2", "python3-smmap"),
    ("SQLAlchemy", "python3-sqlalchemy"),
    ("diskimage-builder", "diskimage-builder"),
    ("Paste", "python3-paste"),
    ("WebOb", "python3-webob"),
    ("dogpile.cache", "python3-dogpile-cache"),
    ("repoze.lru", "python3-repoze-lru"),
    ("CherryPy", "python3-cherrypy"),
    ("MarkupSafe", "python3-markupsafe"),
    ("Routes", "python3-routes"),
    ("backports.functools-lru-cache", "python3-backports-functools-lru-cache"),
    ("jaraco.functools", "python3-jaraco-functools"),
    ("zc.lockfile", "python3-zc-lockfile"),
    ("python-daemon", "python3-daemon"),
    ("python-dateutil", "python3-dateutil"),
    ("python-editor", "python3-editor"),
    ("python-string-utils", "python3-string-utils"),
    ("ruamel.yaml", "python3-ruamel")
  ]
