#!/usr/bin/env bash

export PYTHON=`which python`;

export TEST_PATH="../../";

if [[ $? != 0 ]]; then
  export PYTHON=python
fi

# color output 
export red=$'\e[1;31m';
export green=$'\e[1;32m';
export brown=$'\e[0;33m';
export yellow=$'\e[1;33m';
export blue=$'\e[0;34m';
export light_blue=$'\e[1;34m';
export magenta=$'\e[1;35m';
export cyan=$'\e[1;36m';
export white=$'\e[0m';

declare -i runs=0;

function test_help {
  echo -e "${white}options:";
  echo -e " -show_line - show line numbers";
  # echo -e " -coverage  - Add test coverage";
  # echo -e " -profile   - Add test profiling";
  # echo -e " -debug     - Produce the original output - before the output is canonicalized";
  echo -e " -save_list - Save the current failed list to a file";
  echo -e " -load_list - Load the tests to run from a file";
  echo -e " -help - Show this help message";
  echo -e "arguments:";
  echo -e " [<test>] - Run single test. Optional.";
  echo -e " [<test> <test> <test>...] - Run multiple tests. Optional.\n";
  echo -e "examples:";
  echo -e " ./test -load_list=test_list.txt";  
  echo -e " ./test csv_input_test";  
  echo -e " ./test csv_input_test input_test kvstore_input_test";
  exit;
}

function stop_tests {
  echo -e "stop_tests";
  local file=test_files/locks/test.lock;
  rm $file;
  # unset the traps
  trap - SIGTSTP;
  trap - EXIT;
}

function start_tests {
  # make sure that no matter what happens stop_tests gets run
  # SIGTSTP - no ctrl-z
  local file=test_files/locks/test.lock;
  if [[ -f $file ]]; then
    echo -e "${red}The splunk_capture test frame is already being run in "`more $file`;
    exit;
  fi

  trap stop_tests EXIT;
  trap "" SIGTSTP;
  echo -e "${brown}start_tests `date`";
  ( cd ../..; echo -e `pwd` | xargs basename ) > $file;
}

if [[ ! -d test_files/locks/ ]]; then
  mkdir -p test_files/locks/;
fi

start_tests;

# save a list file
function save_list {
  local file=$1;
  if [[ -z $file ||  $file =~ 'save_list' ]]; then
    echo -e "invalid save_list - requires save_list=filename";
    test_help;
  fi

  if [[ -f $file ]]; then
    echo -e "overwrite existing file? (y/n) ";
    read input
    if [ $input != 'y' ]; then
      echo -e exiting;
      exit;
    fi
  fi
  echo -e "creating file $file";
  ( cd build/test/diffs/; echo -e * ) > $file
  exit;
}

# load a list file
function load_list {
  local file=$1;
 
  if [[ ! -f $file ]]; then
    echo -e "${red}-load_list invalid filename ${file}\n";
    exit;
  fi
  
  while read -r LINE; do
    export THE_TEST="$THE_TEST $LINE"    
  done < $file
  export TEST_LIST=1
}
  

export NO_LINE=true;

CUR_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

for opt in "$@"; do
  if [[ $opt =~ "-help" || $opt =~ "-h" ]]; then  # run only the tests saved in this file
    test_help;
  elif [[ $opt == "-show_line" ]]; then  # show line numbers
    export NO_LINE=false;
  # elif [[ $opt == "-coverage" ]]; then  # Add coverage
  #   export COVERAGE=build/test/cover.data;
  #   export PHP="$PHP -c /org/seg/services/mxd/mxd.csg/web_bin/etc/php.xdebug.ini"
  # elif [[ $opt == "-profile" ]]; then  
  #   export PROFILE=1
  #   export PHP="$PHP -c /org/seg/services/mxd/mxd.csg/web_bin/etc/php.xdebug.profile.ini"
  elif [[ $opt == "-debug" ]]; then  # Both the above
    export NO_LINE=false;
  elif [[ $opt =~ "-save_list" ]]; then  # save the current failed list to a file
    save_list ${opt#*=};
  elif [[ $opt =~ "-load_list" ]]; then  # run only the tests saved in this file
    load_list ${opt#*=};
  elif [[ ${opt:0:1} != '-' ]]; then 
    if [[ -n $THE_TEST ]]; then  # already created
      export THE_TEST="$THE_TEST $opt";
      export TEST_LIST=1;
    else 
      export THE_TEST=$opt;
    fi
  else
    echo -e "${red}\nUnknown option ${opt}\n";
    test_help;
  fi
done

if [[ -n $TEST_LIST ]]; then 
  declare -a THE_TEST=($THE_TEST);
  export THE_TEST=${THE_TEST[@]};
  echo -e "${cyan}Testing List: $THE_TEST";
fi

function check_known_failures {
  if [[ -f known_failures/$2 ]]; then 
    let invalid_known_fail=$invalid_known_fail+1;
    echo -e "${green}TEST PASS -- KNOWN FAIL FILE EXISTS";
  fi
}

function set_fail {
  diff $1 known_failures/$2 &> /dev/null
  if [[ $? == 0 ]]; then 
    echo -e "${yellow}known failure "
    let known_fail=$known_fail+1;
  else 
    let fail=$fail+1;
    local base=`basename $1`;
    if [ -f known_failures/$2 ]; then  # should have been a failure
      echo -e "${red}KNOWN FAILURE -- FAILED";
      echo -e $base > build/test/known_failure_diffs/$base;
      diff $1 known_failures/$2 >> build/test/known_failure_diffs/$base 2>&1;
      cp known_failures/$2 build/test/html/$base.known_failure.html;
    else
      echo -e "${red}FAILED";
    fi

    echo -e $base > build/test/diffs/$base
    diff $1 golden_results/$2 | cat -t >> build/test/diffs/$base 2>&1;
    cp $1 build/test/html/$base.html;
    if [ -f golden_results/$2 ]; then 
      cp golden_results/$2 build/test/html/$base.golden.html;
    fi
  fi 
}

function run_diff {
  runs=$(( runs + 1 ));
  ran=1;

  if [[ $NO_LINE == true ]]; then 
    if [[ -f golden_results/$2 ]]; then
      echo "${cyan}diff ${1} golden_results/${2}";          
      diff $1 golden_results/$2 > /dev/null;
    fi
  fi  
  
  if [[ $? == 0 ]]; then # pass
    echo -e "${green}PASSED";
    check_known_failures $1 $2;
  else
    echo "${red} $1 FAILED";
    set_fail $1 $2;
  fi
}

function run_phantomjs {
  local file=$1;
  local tmp=phantomjs_temp_file.html;
  local path=test/files_for_test/phantomjs;

  cp $file ../$tmp;
  
  perl -0777 -i.bak -pe 's#/gizmos#/site/austin/org-seg-services-mxd/mxd.csg.apple.com/content/gizmos#sg' ../$tmp;
 ( 
   cd ..
   local my_pwd=`pwd`;
   perl -0777 -i.bak -pe "s#mxd2/javascript#$my_pwd/latest/mxd2/javascript#sg" $tmp;
 )
  # to test, use --debug=true
  (cd ..; $path/phantomjs --ssl-protocol=any $path/$PHANTOMJS $tmp ) &> $file
  rm ../$tmp
}

function check_test {
  if [[ -n $TEST_LIST ]]; then
    for i in ${THE_TEST[@]}; do 
      # =~ changes with the order of the args, i.e., both directions are necessary
      if [[ $i == $1  || $1 =~ $i || $i =~ $1 ]]; then 
        return 0;
      fi
    done
  else
    # =~ changes with the order of the args, i.e., both directions are necessary
    if [[ -z $THE_TEST || $THE_TEST == $1  || $1 =~ $THE_TEST || $THE_TEST =~ $1 ]]; then 
      return 0;
    fi
  fi
  return 1;
}

function run { 
  check_test $2 || return 1; 
  
  local options=$1;
  local test_file=$2;
  local base=golden_results/$2;
  local file=build/test/results/$test_file;  
  local custom_command="";

  echo -e "\n${magenta}****************************************";
  echo -e "${white}testing $base";
  echo -e "${magenta}****************************************";

  local command=("./splunk_capture" "$options");

  if [ -f "tests/${test_file}_custom" ]; then      
    custom_command=`cat tests/${test_file}_custom`;
    echo -e "${light_blue}tests/${test_file}_custom found: $custom_command";
    command=("./splunk_capture" "$options" "$custom_command");
  fi

  echo "${white}${command[@]}";

  echo -e $blue;
  ( cd $TEST_PATH; eval "${command[@]}" ) &> $file  

  if [[ -n $3 ]]; then # call callback
    $3 >> $file;
  fi
  if [[ -n $PHANTOMJS ]]; then
    run_phantomjs $file;
  fi
  run_diff $file $test_file;
  echo -e "${magenta}----------------------------------------";
  return 0;
}

function start {
  let time_start1=`date +"%s"`;
  let fail=0;
  let known_fail=0;
  let invalid_known_fail=0;  

  export TEST_PATH="../..";

  if [[ $invalid_known_fail -ne 0 ]]; then 
    echo -e "${red}# invalid known failure files " $invalid_known_fail; 
  fi;

  if [[ $fail -ne 0 || $known_fail -ne 0 ]]; then 
    echo -e "${red}# of unexpected failures " $fail; 
    echo -e "${yellow}# of known failures " $known_fail;
  fi;

  # run test against golden results
  # there should be the same number of test files as there are golden results
  let count=0; 
  for i in $THE_TEST; 
    do 
      let count=$count+1;
      local file_name=`echo -e $i | sed 's:.*/::'`;
      local test_options=`cat tests/$file_name`
      run "$test_options" $file_name;
  done

  let time_start2=`date +"%s"`;
  let time_start1=$time_start2-$time_start1;

  echo $green;
  echo -e "seconds $time_start1";

  echo -e "# of runs " $runs;

  if [[ $fail -eq 0 ]]; then 
    echo -e "${green}** test frame passed **";
    echo -e "${green}# of unexpected failures " $fail; 
    echo -e "${green}# of known failures " $known_fail;
  fi;

  if [ $runs -ne $count ]; then 
    let x=$count-$runs;
    echo -e "${red}NOT RUN $x";
  fi
  date;
}

echo -e "\n${magenta}creating fresh build directory";
echo -e "${magenta}running tests from root of splunk_capture repo";
rm -rf build;

if [[ ! -d build/test/results ]]; then
  mkdir -p build/test/results;
fi
if [[ ! -d build/test/diffs ]]; then
  mkdir -p build/test/diffs;
fi
if [[ ! -d build/test/html/ ]]; then
  mkdir -p build/test/html/
fi
if [[ ! -d build/test/known_failure_diffs ]]; then
  mkdir -p build/test/known_failure_diffs;
fi

if [[ -z $NO_RUN ]];  then 
  rm -f  build/test/results/*;

  start;
fi

# reset default color scheme
echo -en "\e[0m";