{ emacsmirror, github, fetchzip, ... }: eself: esuper: {
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
