#!/bin/bash

echo "Content-type: text/html"
echo ""
echo "<p>Running new build</p>"
mkdir -p ../files 
cd ../../../
echo $(ls)
git pull
export JAVA_HOME=/usr/lib/jvm/default
./configure --disable-server --with-qt-path=/opt/Qt/ --with-arch=arm64_v8a ANDROID_NDK_ROOT=/home/jamesh/Android/android-ndk-r19c/ ANDROID_SDK_ROOT=/home/jamesh/Android/sdk/ JAVA_HOME=/usr/lib/jvm/default/
make -j
mv ProjectM.apk Server/root/files/ProjectM-$(echo -n `date '+%Y_%m_%d__%H_%M_%S'`).apk
