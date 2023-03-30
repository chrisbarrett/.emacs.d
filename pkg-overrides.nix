{ emacsmirror, github, fetchzip, ... }: eself: esuper: {
  chatgpt-shell = github {
    name = "chatgpt-shell";
    owner = "xenodium";
    rev = "0403d967308180414c121a24fa837a4cd3d1c5a3";
    sha256 = "sha256-UpnMKRmHiEfVmTwr6su6/4bz/F+3wtd6MBUXnfFPgFU=";
    buildInputs = [ esuper.markdown-mode ];
  };

  eglot-x = github {
    name = "eglot-x";
    owner = "nemethf";
    rev = "08cbd4369618e60576c95c194e63403f080328ba";
    sha256 = "sha256-cWicqHYR/XU+71a8OFgF8vc6dmT/Fy0EEgzX0xvYiDc=";
    buildInputs = [ esuper.eglot ];
  };

  git-gutter = github {
    name = "git-gutter";
    owner = "emacsorphanage";
    rev = "1451e3149865b88de2402ce4296ee5608fadc5b2";
    sha256 = "sha256-3obNSE47GY2zbo/iSSvfhrr51JC5B+0HFjJNwYET7AI=";
  };

  org-transclusion = github {
    name = "org-transclusion";
    owner = "nobiot";
    rev = "ccc0aaa72732ea633bf52bcc8a0345cd3ac178fd";
    sha256 = "0sqs5gi58ngrjqpx35ff4q6lhgz32bcsxb9jkamw0rn18qp92w3p";
  };

  org-pretty-table = github {
    name = "org-pretty-table";
    owner = "Fuco1";
    rev = "7bd68b420d3402826fea16ee5099d04aa9879b78";
    sha256 = "1lxs94318mvp49swrrb4p87b61qsy61ak8al3msqv1am69wyflsf";
  };

  hide-comnt = emacsmirror {
    name = "hide-comnt";
    rev = "d1e94f5152f20b2dc7b0d42898c1db37e5be57a6";
    sha256 = "002i9f97sq3jfknrw2nim1bhvj7xz3icviw7iffqmpmww4g1hq9l";
  };

  info-plus = emacsmirror {
    name = "info-plus";
    rev = "4a6b93c170169594e1e8ea60cd799a1a88a969da";
    sha256 = "1xzmx7m1qbl3b1x6yq1db1a108xqaa64ljfv1hdw763zmy4kc6m0";
  };

  csharp-mode = esuper.csharp-mode.overrideAttrs (it: {
    patches = [ ./patches/csharp-mode-keywords.patch ];
  });
}
