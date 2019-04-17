AC_INIT([project-m], [0.1])

AC_ARG_WITH([qt-path],
            AS_HELP_STRING([--with-qt-path], [Manually set path to Qt installation]),
            qt_result="$withval",
            qt_result="no")

AC_ARG_WITH([arch],
            AS_HELP_STRING([--with-arch], [Set android build architecture. Options are arm64_v8a, armv7, x86]),
            arch_result="android_$withval",
            arch_result="no")

AC_ARG_WITH([qt-version],
            AS_HELP_STRING([--with-qt-version], [Set Qt version to use]),
            version_result="$withval",
            version_result="no")

AC_ARG_ENABLE([app],
              AS_HELP_STRING([--disable-app], [Do not build app]),
              build_app="no",
              build_app="yes")

AC_ARG_ENABLE([server],
              AS_HELP_STRING(--disable-server, [Do not build server]),
              build_server="no",
              build_server="yes")

AC_ARG_WITH([live-address],
              AS_HELP_STRING([--with-live-address], [Set live server address for app]),
              server_addr="$withval",
              server_addr="localhost:3000")

AC_ARG_ENABLE([autodep],
              AS_HELP_STRING([--enable-autodep],[Auto-install haskell dependencies (needs cabal)]),
              auto_dep="yes",
              auto_dep="no")

AC_ARG_VAR([ANDROID_NDK_ROOT],[android ndk path])
AC_ARG_VAR([ANDROID_SDK_ROOT],[android sdk path])
AC_ARG_VAR([JAVA_HOME],[path to jdk path])

AC_SUBST([jdk],[$JAVA_HOME])

if test "$build_app" == "yes"; then
  if test "$qt_result" == "no"; then
    AC_MSG_CHECKING([for qmake])
    if test -z $(which qmake); then
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Unable to find qmake. Try setting --qt-path])
      exit 1
    else
      AC_MSG_RESULT([yes])
      qmake_exec=qmake
      AC_SUBST([qmake_exec])
    fi
    AC_MSG_CHECKING([for androiddeployqt])
    if test -z $(which androiddeployqt); then
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Unable to find androiddeployqt. Try setting --qt-path])
      exit 1
    else
      AC_MSG_RESULT([yes])
      deploy_tool=androiddeployqt
      AC_SUBST([deploy_tool])
    fi

  else
    AC_MSG_CHECKING([if user defined qt-path is valid])
    if test -d "$qt_result"; then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_ERROR([Qt Path is not valid])
      exit 1
    fi
    AC_MSG_CHECKING([what version of qt to use])
    if test "$version_result" == "no"; then
      version_result=$(ls -1a $qt_result | grep -E '5(\.[0-9]+)*' | tail -n 1)
    fi
    AC_MSG_RESULT([Using newst installed ($version_result)])
    AC_MSG_CHECKING([if arch is set])
    if test "$arch_result" == "no"; then
      AC_MSG_RESULT([no])
      AC_MSG_CHECKING([if arch can be deduced. Please set --with-arch])
      a=$(ls -d $qt_result/$version_result/android_* | wc -l)
      if test "$a" != "1"; then
        AC_MSG_RESULT([no])
        AC_MSG_ERROR([Cannot deduce arch])
        exit 1
      else
        AC_MSG_RESULT([yes])
        arch_result=$a
      fi
    else
      AC_MSG_RESULT([yes])
    fi
    AC_MSG_CHECKING([for qmake])
    qmake_dir=$qt_result/$version_result/$arch_result/bin
    qmake_exec=$qmake_dir/qmake
    if test -x $qmake_exec; then
      AC_MSG_RESULT([exists])
      AC_SUBST([qmake_exec])
    else
      AC_MSG_ERROR([cannot find qmake])
      exit 1
    fi
    AC_MSG_CHECKING([for androiddeployqt])
    deploy_tool=$qmake_dir/androiddeployqt
    if test -x $deploy_tool; then
      AC_MSG_RESULT([exists])
      AC_SUBST([deploy_tool])
    else
      AC_MSG_ERROR([cannot find androiddeployqt])
      exit 1
    fi
  fi

  AC_MSG_NOTICE([Building mobile app makefiles])
  if test -d build; then rm -fr build; fi
  mkdir -p build
  $qmake_exec App/ProjectM/ProjectM.pro -o build/Makefile 'DEFINES+=PROJECT_BASE_IP=\\\"$server_addr\\\"'
fi

if test "$build_server" == "yes"; then
  if test "$auto_dep" == "yes"; then
    AC_MSG_NOTICE([Starting haskell dependencies install])
    AC_MSG_CHECKING([for ghc-pkg])
    if test -z $(which ghc-pkg); then
      AC_MSG_ERROR([Can't find ghc-pkg. Please install GHC])
      exit 1
    else
      AC_MSG_RESULT([found])
      pkg_ins=""
      AC_MSG_CHECKING([for strict])
      if test -z $(ghc-pkg list | grep strict); then
        AC_MSG_RESULT([Marked for install])
        pkg_ins="$pkg_ins strict"
      else
        AC_MSG_RESULT([found])
      fi
      AC_MSG_CHECKING([for hedis])
      if test -z $(ghc-pkg list | grep hedis); then
        AC_MSG_RESULT([Marked for install])
        pkg_ins="$pkg_ins hedis"
      else
        AC_MSG_RESULT([found])
      fi
      if test "$pkg_ins" == ""; then
        AC_MSG_NOTICE([Nothing to install])
      else
        AC_MSG_NOTICE([Installing $pkg_ins])
        AC_MSG_CHECKING([for cabal])
        if test -z $(which cabal); then
          AC_MSG_ERROR([Couldn't find cabal. Please install])
          exit 1
        else
          AC_MSG_RESULT([Found])
          AC_MSG_NOTICE([Updating cabal pakage list])
          cabal update
          AC_MSG_NOTICE([Installing $pkg_ins])
          cabal install $pkg_ins
        fi
      fi
    fi
  fi
  if test "$auto_dep" == "no"; then
    AC_MSG_CHECKING([for ghc-pkg])
    if test -z $(which ghc-pkg); then
      AC_MSG_ERROR([Can't find ghc-pkg. Please install GHC])
      exit 1
    else
      AC_MSG_RESULT([found])
      AC_MSG_CHECKING([for strict])
      if test -z $(ghc-pkg list | grep strict); then
        AC_MSG_ERROR([Could not find haskell pakage strict. Please install or enable auto dependency install with --enable-autodep])
      else
        AC_MSG_RESULT([found])
      fi
      AC_MSG_CHECKING([for hedis])
      if test -z $(ghc-pkg list | grep hedis); then
        AC_MSG_ERROR([Could not find haskell pakage hedis. Please install or enable auto dependency install with --enable-autodep])
      else
        AC_MSG_RESULT([found])
      fi
    fi
  fi
fi

targets=""
if test "$build_app" == "yes"; then
  targets="$targets ProjectM.apk"
fi
if test "$build_server" == "yes"; then
  targets="$targets main.cgi"
fi
AC_SUBST([targets])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
