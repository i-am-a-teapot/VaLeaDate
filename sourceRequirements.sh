
mkdir -p requirements
cd requirements

if [ ! -f graalvm-jdk-25_linux-x64_bin.tar.gz ]; then
    echo "Downloading GraalVM JDK 25..."
    curl https://download.oracle.com/graalvm/25/latest/graalvm-jdk-25_linux-x64_bin.tar.gz -O
fi

graalvmDir=$(find . -maxdepth 1 -type d -name "graalvm-jdk-25*" | head -n 1)

if [ -z "$graalvmDir" ]; then
    tar -xzf graalvm-jdk-25_linux-x64_bin.tar.gz
    graalvmDir=$(find . -maxdepth 1 -type d -name "graalvm-jdk-25*" | head -n 1)
fi

export JAVA_HOME=$(pwd)/$graalvmDir
export PATH=$JAVA_HOME/bin:$PATH

SBT_VERSION="1.10.7"

if [ ! -f sbt-${SBT_VERSION}.tgz ]; then
    echo "Downloading SBT ${SBT_VERSION}..."
    curl -L https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz -O
    echo "Extracting SBT..."
    tar -xzf sbt-${SBT_VERSION}.tgz
fi

# Add SBT to the PATH
export PATH=$(pwd)/sbt/bin:$PATH


if [ ! -f $HOME/.elan/env ]; then
    echo "Downloading and Installing Lean"
    curl https://elan.lean-lang.org/elan-init.sh -sSf | sh -s -- -y
fi

source $HOME/.elan/env
cd ..