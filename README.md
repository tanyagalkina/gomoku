# gomoku

https://gomocup.org/detail-information/

## runing piskvork on MacOS

brew install wine-stable
https://sourceforge.net/projects/piskvork/

## Cross compilation


docker pull koalaman/winghc

docker run -dit -v "$PWD":/workspace -w /workspace --name my_haskell_container koalaman/winghc bash

docker exec -it <container_id> /bin/bash

cabal build

### getting stack

curl -sSL https://get.haskellstack.org/ | sh

docker cp my_haskell_container:/workspace/dist/build/gomoku-exe/gomoku-exe.exe pbrain-tag43.exe
cp pbrain-tag43.exe ../../../Downloads/piskvork/


