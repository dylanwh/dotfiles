{
  config,
  pkgs,
  ...
}:

let
  gh = pkgs.fetchFromGitHub;
in
{
  home.file = {
    # Vim runtime dirs from dotfiles repo
    ".vim/vimrc".source = ../../vim/vimrc;
    ".vim/after".source = ../../vim/after;
    ".vim/autoload".source = ../../vim/autoload;
    ".vim/colors".source = ../../vim/colors;
    ".vim/compiler".source = ../../vim/compiler;
    ".vim/doc".source = ../../vim/doc;
    ".vim/ftdetect".source = ../../vim/ftdetect;
    ".vim/ftplugin".source = ../../vim/ftplugin;
    ".vim/indent".source = ../../vim/indent;
    ".vim/plugin".source = ../../vim/plugin;
    ".vim/snippets".source = ../../vim/snippets;
    ".vim/spell".source = ../../vim/spell;
    ".vim/syntax".source = ../../vim/syntax;
    ".vim/theme.vim".text = ''
      set bg=${config.selenized.vimBg}
      colorscheme ${config.selenized.vimColorscheme}
      let g:lightline = {  'colorscheme': 'selenized_${config.selenized.variant}' }
    '';

    # Plugins in pack/dylan/start
    ".vim/pack/dylan/start/vim-abolish".source = gh {
      owner = "tpope";
      repo = "vim-abolish";
      rev = "3f0c8faadf0c5b68bcf40785c1c42e3731bfa522";
      sha256 = "1w9zim2v1av3f43z8q7zh0ia8dgjxjwnvmzd4j3y25vy25avn0lb";
    };
    ".vim/pack/dylan/start/ag.vim".source = gh {
      owner = "epmatsw";
      repo = "ag.vim";
      rev = "6ac76d95cdd7c6978bfadada8db37fc9e32b5dbb";
      sha256 = "11ypb7qg1h8mrwbrwvlaclmb6hp7j7mwzk06ir6f5zcf5q70560i";
    };
    ".vim/pack/dylan/start/ale".source = gh {
      owner = "dense-analysis";
      repo = "ale";
      rev = "f78e9d634f9c1177031d4bdeda93f98d63b6bc12";
      sha256 = "15bpbm0w9crw8pgxfynlvp9ccqzbjibgk4p1pj5fix7vzlhchh2w";
    };
    ".vim/pack/dylan/start/vim-applescript".source = gh {
      owner = "dearrrfish";
      repo = "vim-applescript";
      rev = "91a5af926da819dd9d812ed79a8aac6bbdedeee9";
      sha256 = "0711zf2l2nvmzlzmxqgnsg1dhc25mq9l0m40d7wggg8z41vbf61g";
    };
    ".vim/pack/dylan/start/vim-commentary".source = gh {
      owner = "tpope";
      repo = "vim-commentary";
      rev = "e87cd90dc09c2a203e13af9704bd0ef79303d755";
      sha256 = "09kzc89iwkgsi4wvjxk56fis462kkz5chcl9sl4hdbmpa1f41wy0";
    };
    ".vim/pack/dylan/start/copilot.vim".source = gh {
      owner = "github";
      repo = "copilot.vim";
      rev = "5a411d19ce7334ab10ba12516743fc25dad363fa";
      sha256 = "1v72i3f1w7q481grffm2grb9m11qiazs85xq3j89yiqh6jvvpvzh";
    };
    ".vim/pack/dylan/start/vim-cpanfile".source = gh {
      owner = "moznion";
      repo = "vim-cpanfile";
      rev = "d69eb9aee5c60da6ff68b8fc3058b69bf5a4f6d9";
      sha256 = "10cnw8wikfcvmx4fsjwnvgky24z0vqf6n59wgfijkpabvnjdpi79";
    };
    ".vim/pack/dylan/start/ctrlp.vim".source = gh {
      owner = "ctrlpvim";
      repo = "ctrlp.vim";
      rev = "8b4a9523632049b3b373de1233bef346073b8982";
      sha256 = "0g7mymqgncnmc326xysx9rlhja5601b18sn4hbpr7p49sqqspyv4";
    };
    ".vim/pack/dylan/start/editorconfig-vim".source = gh {
      owner = "editorconfig";
      repo = "editorconfig-vim";
      rev = "30ddc057f71287c3ac2beca876e7ae6d5abe26a0";
      sha256 = "1a3k9daskd2c09mzb3hbbf13gs50xk19w6w4ggxwb4dcr37x93hm";
    };
    ".vim/pack/dylan/start/vim-endwise".source = gh {
      owner = "tpope";
      repo = "vim-endwise";
      rev = "4e5c8358d751625bb040b187b9fe430c2b769f0a";
      sha256 = "0vfkvq20kr6jbm526vkvyliv4zybha7jpp7iaimvw4zx1mjs5pih";
    };
    ".vim/pack/dylan/start/vim-eunuch".source = gh {
      owner = "tpope";
      repo = "vim-eunuch";
      rev = "291ef1f8c8996ca7715df1032a35a27b12d7b5cf";
      sha256 = "0fy40dh98z5rmhja6czaxwv48lfpph71vbqsfz55119686zgmc5p";
    };
    ".vim/pack/dylan/start/vim-exchange".source = gh {
      owner = "tommcdo";
      repo = "vim-exchange";
      rev = "784d63083ad7d613aa96f00021cd0dfb126a781a";
      sha256 = "15bwlky7if7jp3g3banxi2kkci1wa54y36b4m27mggsc6kdabd6q";
    };
    ".vim/pack/dylan/start/vim-fish".source = gh {
      owner = "dag";
      repo = "vim-fish";
      rev = "50b95cbbcd09c046121367d49039710e9dc9c15f";
      sha256 = "1yvjlm90alc4zsdsppkmsja33wsgm2q6kkn9dxn6xqwnq4jw5s7h";
    };
    ".vim/pack/dylan/start/vim-fugitive".source = gh {
      owner = "tpope";
      repo = "vim-fugitive";
      rev = "23b9b9b2a3b88bdefee8dfd1126efb91e34e1a57";
      sha256 = "0spghyavc5vxja7b2apck501j61s8gc43bqv1388akwmdanmbk1p";
    };
    ".vim/pack/dylan/start/vim-gitgutter".source = gh {
      owner = "airblade";
      repo = "vim-gitgutter";
      rev = "400a12081f188f3fb639f8f962456764f39c6ff1";
      sha256 = "1rfg2wniappm64y5b6j6nmqsp41axgs6f3ghdzcjgak71b6mbng9";
    };
    ".vim/pack/dylan/start/vim-go".source = gh {
      owner = "fatih";
      repo = "vim-go";
      rev = "0550762a69a2ef13d9f723c9478155dbc6ad7ddb";
      sha256 = "185ysbbipqf6fivryi4pxjmdm4vfn319w1mgm3xpg2920scybwbq";
    };
    ".vim/pack/dylan/start/vim-groovy".source = gh {
      owner = "thecodesmith";
      repo = "vim-groovy";
      rev = "b78bbbbc2eea6499c4ad3d092ece68216a89af73";
      sha256 = "0sv9lfk3zkkm1bp5qvpwlby9hfyn9g689qcf56k1s8a1ybac3ziz";
    };
    ".vim/pack/dylan/start/html5.vim".source = gh {
      owner = "othree";
      repo = "html5.vim";
      rev = "7c9f6f38ce4f9d35db7eeedb764035b6b63922c6";
      sha256 = "1hgbvdpmn3yffk5ahz7hz36a7f5zjc1k3pan5ybgncmdq9f4rzq6";
    };
    ".vim/pack/dylan/start/vim-jq".source = gh {
      owner = "bfrg";
      repo = "vim-jq";
      rev = "5b6f0780c1be0350930d95e8e34536f5d2592aa5";
      sha256 = "11a9snq8b07xlqmc7yi8mvsny6mgmyxj9b5l39jdd7s9z275ksvf";
    };
    ".vim/pack/dylan/start/vim-jsonnet".source = gh {
      owner = "google";
      repo = "vim-jsonnet";
      rev = "f27e1d67dff34ac9cf9c83ea2f2f814e53aa409c";
      sha256 = "05zlsn7344zffwjsx4f65rp85jrgfm4wi40iqjj137x11jhwb9a2";
    };
    ".vim/pack/dylan/start/lightline.vim".source = gh {
      owner = "itchyny";
      repo = "lightline.vim";
      rev = "b1e91b41f5028d65fa3d31a425ff21591d5d957f";
      sha256 = "0xb0hdjk4dww80s2ypvgz5rsvv41b07hskahz8r7xq6si5m5scrn";
    };
    # ".vim/pack/dylan/start/lsp".source = gh {
    #   owner = "yegappan";
    #   repo = "lsp";
    #   rev = "5205964bd3b812dc24f9a867d002f74192325d10";
    #   sha256 = "18vhz9jd5i9sb1yd4dyds0fq0vhxdbf93303z5353fzz1pxij09a";
    # };
    ".vim/pack/dylan/start/vim-osc52".source = gh {
      owner = "fcpg";
      repo = "vim-osc52";
      rev = "551f20e62e68684a5b745ae08b0c4236d86e4c2b";
      sha256 = "0yxjs32ab27l3kmh5lpinj15m11winqsnmykjq7lizs15v7gd2s9";
    };
    ".vim/pack/dylan/start/vim-perl".source = gh {
      owner = "vim-perl";
      repo = "vim-perl";
      rev = "f40ece108099df561157ed7fdb764f8c183a8bfc";
      sha256 = "0lyypxavnqrhn9zghjd2s96f9xkn4izn85abbi6gfbmd587apzjk";
    };
    ".vim/pack/dylan/start/python-syntax".source = gh {
      owner = "vim-python";
      repo = "python-syntax";
      rev = "2cc00ba72929ea5f9456a26782db57fb4cc56a65";
      sha256 = "1w4yd25rnbhsps81agvq0kr3vcbifrlpb7h4z0vcgsx1nvmxy205";
    };
    ".vim/pack/dylan/start/vim-racket".source = gh {
      owner = "wlangstroth";
      repo = "vim-racket";
      rev = "32ad23165c96d05da7f3b9931d2889b7e39dcb86";
      sha256 = "1yyqx471p11vj6gya4yzkiy07vfwzpx10bf6s7dh2h7zp2nz10br";
    };
    ".vim/pack/dylan/start/vim-rails".source = gh {
      owner = "tpope";
      repo = "vim-rails";
      rev = "a6d2bac95b446a5c23eff4638eca164b0d77454a";
      sha256 = "0mxc8jv8y7dfy8a14y3xbgs75fv8443nnpf27wk4bz8vdp6nbfha";
    };
    ".vim/pack/dylan/start/vim-repeat".source = gh {
      owner = "tpope";
      repo = "vim-repeat";
      rev = "24afe922e6a05891756ecf331f39a1f6743d3d5a";
      sha256 = "0y18cy5wvkb4pv5qjsfndrpcvz0dg9v0r6ia8k9isp4agdmxkdzj";
    };
    ".vim/pack/dylan/start/vim-rhubarb".source = gh {
      owner = "tpope";
      repo = "vim-rhubarb";
      rev = "cad60fe382f3f501bbb28e113dfe8c0de6e77c75";
      sha256 = "1c456ypxgjnmbprjl79kvg5vm26nws0csw8fbs6gmdr21m3x1p4w";
    };
    ".vim/pack/dylan/start/vim-ripgrep".source = gh {
      owner = "jremmen";
      repo = "vim-ripgrep";
      rev = "2bb2425387b449a0cd65a54ceb85e123d7a320b8";
      sha256 = "0vzdi1q6xh440mvlgsi08d6py8n7ac2yhgixpcs74f4p9160zx1s";
    };
    ".vim/pack/dylan/start/rust.vim".source = gh {
      owner = "rust-lang";
      repo = "rust.vim";
      rev = "1cdc5cb4bd061bc30b502e07321682b5a4396dca";
      sha256 = "1xklq4g6dvqd3a5msnrir899hzqmjkscpb8bq9wd4fi3ivmjjjgg";
    };
    ".vim/pack/dylan/start/splitjoin.vim".source = gh {
      owner = "AndrewRadev";
      repo = "splitjoin.vim";
      rev = "956d67c88512b91c2938da3a9e0aeeea2b82cd0b";
      sha256 = "0h01mvwdxlfsi1af0k53fqrf5zrv6cq15bcrhfffzlg9wp81pcvy";
    };
    ".vim/pack/dylan/start/starlark.vim".source = gh {
      owner = "cappyzawa";
      repo = "starlark.vim";
      rev = "55b48a87066a142faa51bbc0548a46d1575d15f0";
      sha256 = "108hfjcvl0xl6m2b544pz5d9h573qhsj7s1hfj2kmm830657n4jd";
    };
    ".vim/pack/dylan/start/supertab".source = gh {
      owner = "ervandew";
      repo = "supertab";
      rev = "f0093ae12a9115498f887199809a6114659fc858";
      sha256 = "1r1ck62raxhwcxjv7hic7jm9b5inm1vdqc51zn4nybclys7grnss";
    };
    ".vim/pack/dylan/start/vim-surround".source = gh {
      owner = "tpope";
      repo = "vim-surround";
      rev = "3d188ed2113431cf8dac77be61b842acb64433d9";
      sha256 = "0lfnhwcgfxcg4n59xjxc29fhj08jhdv41l8zpx0fjkx796v3k48d";
    };
    ".vim/pack/dylan/start/tabular".source = gh {
      owner = "godlygeek";
      repo = "tabular";
      rev = "339091ac4dd1f17e225fe7d57b48aff55f99b23a";
      sha256 = "0jq91770wsp2waw7pywxjqx59v0vg54gfzirgcd85pg61w22bfla";
    };
    ".vim/pack/dylan/start/vim-terraform".source = gh {
      owner = "hashivim";
      repo = "vim-terraform";
      rev = "f0b17ac9f1bbdf3a29dba8b17ab429b1eed5d443";
      sha256 = "0j87i6kxafwl8a8szy2gzv7d0qhzwynd93iw8k0i42jnpqm8rp3a";
    };
    ".vim/pack/dylan/start/vim-textobj-rubyblock".source = gh {
      owner = "nelstrom";
      repo = "vim-textobj-rubyblock";
      rev = "2b882e2cc2599078f75e6e88cd268192bf7b27bf";
      sha256 = "197m0a45ywdpq2iqng6mi5qjal2qggjjww1k95iz2s7wvh8ng9yr";
    };
    ".vim/pack/dylan/start/vim-textobj-user".source = gh {
      owner = "kana";
      repo = "vim-textobj-user";
      rev = "41a675ddbeefd6a93664a4dc52f302fe3086a933";
      sha256 = "1y1g3vcm97fqjyigiajbvbck4nlc04vxl3535x4sl40s5jbm5vz3";
    };
    ".vim/pack/dylan/start/vim-unimpaired".source = gh {
      owner = "tpope";
      repo = "vim-unimpaired";
      rev = "efdc6475f7ea789346716dabf9900ac04ee8604a";
      sha256 = "1lwi0dpp5jrrnzjwrl4qs88a0q4gfgiqlpjcz62j5hmmdcbj5j4s";
    };
    ".vim/pack/dylan/start/vim-vinegar".source = gh {
      owner = "tpope";
      repo = "vim-vinegar";
      rev = "bb1bcddf43cfebe05eb565a84ab069b357d0b3d6";
      sha256 = "0836s6dl12qdk6x9ihp30h8w6f5lmbs28vmp0njza5fi0m7xb4df";
    };
    ".vim/pack/dylan/start/vim-solarized8".source = gh {
      owner = "lifepillar";
      repo = "vim-solarized8";
      rev = "9f9b7951975012ce51766356c7c28ba56294f9e8";
      sha256 = "1qg9n6c70jyyh38fjs41j9vcj54qmhkkyzna0la7bwsycqfxbs2x";
    };

    # Point nvim config to vim config
    ".config/nvim/init.vim".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.vim/vimrc";
  };
}
