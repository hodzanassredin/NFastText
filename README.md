# NFastText

NFastText is a .net library for efficient learning of word representations and sentence classification.

##[Tutorial](https://hodzanassredin.github.io/NFastText/)

##Ubuntu Quick Start
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
    echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
    sudo apt-get update
    sudo apt-get install mono-devel fsharp
    sudo cert-sync /etc/ssl/certs/ca-certificates.crt
    
    git clone https://github.com/hodzanassredin/nfastText.git
    cd nfastText
    ./build.sh
    ./load_test_data.sh
    fsharpi fasttext.fsx

## Build Status

Mono | .NET
---- | ----
[![Mono CI Build Status](https://travis-ci.org/hodzanassredin/NFastText.svg?branch=master)](https://travis-ci.org/hodzanassredin/NFastText) | [![.NET Build Status](https://ci.appveyor.com/api/projects/status/la2bl0e332ci8ut1?svg=true)](https://ci.appveyor.com/project/hodzanassredin/nfasttext)

## Maintainer(s)

- [@hodzanassredin](https://github.com/hodzanassredin)

