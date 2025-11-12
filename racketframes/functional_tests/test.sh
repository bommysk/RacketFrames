#!/bin/sh

export PYTHON=`which python`;

export TEST_PATH="../";

if [[ $? != 0 ]]; then
  export PYTHON=python
fi

export BASIC_TEST="design_type=basic_test";

function test_help {
  echo "-show_line - show line numbers"
  echo "-coverage  - Add test coverage"
  echo "-profile   - Add test profiling"
  echo "-debug     - Produce the original output - before the output is canonicalized"
  echo "-save_list - Save the current failed list to a file"
  echo "-load_list - Load the tests to run from a file"
  exit
}

function stop_tests {
  echo "stop_tests";
  echo $!;
  kill $!;
  local file=../../test_files/locks/test.lock
  rm $file
  # unset the traps
  trap - SIGTSTP
  trap - EXIT
}

function start_tests {
  # make sure that no matter what happens stop_tests gets run
  # SIGTSTP - no ctrl-z
  local file=../../test_files/locks/test.lock
  if [[ -f $file ]]; then
    echo "The splunk_capture test frame is already being run in "`more $file`
    exit
  fi

  trap stop_tests EXIT
  trap "" SIGTSTP
  echo "start_tests `date`";
  ( cd ..; echo `pwd` | xargs basename ) > $file;
}

if [[ ! -d ../../test_files/locks/ ]]; then
  mkdir -p ../../test_files/locks/;
fi

start_tests;

# save a list file
function save_list {
  local file=$1
  if [[ -z $file ||  $file =~ 'save_list' ]]; then
    echo "invalid save_list - requires save_list=filename"
    test_help
  fi

  if [[ -f $file ]]; then
    echo "overwrite existing file? (y/n) "
    read input
    if [ $input != 'y' ]; then
      echo exiting
      exit
    fi
  fi
  echo "creating file $file";
  ( cd ../build/test/diffs/; echo * ) > $file
  exit
}

# load a list file
function load_list {
  local file=$1
 
  if [[ ! -f $file ]]; then
    echo "-load_list invalid filename $file \n";
    exit;
  fi
  
  while read -r LINE; do
    export THE_TEST="$THE_TEST $LINE"    
  done < $file
  export TEST_LIST=1
}
  

export NO_LINE=true;

CUR_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

for i in "$@"; do
  if [[ $i == "-show_line" ]]; then  # show line numbers
    export NO_LINE=false;
  elif [[ $i == "-debug" ]]; then  # Both the above
    export NO_LINE=false;
  elif [[ $i =~ "-save_list" ]]; then  # save the current failed list to a file
    save_list ${i#*=}
  elif [[ $i =~ "-load_list" ]]; then  # run only the tests saved in this file
    load_list ${i#*=}
  elif [[ ${i:0:1} != '-' ]]; then 
    if [[ -n $THE_TEST ]]; then  # already created
      export THE_TEST="$THE_TEST $i";
      export TEST_LIST=1;
    else 
      export THE_TEST=$i;
    fi
  else
    echo -e "\nUnknown option $i\n"
    test_help
  fi
done
if [[ -n $TEST_LIST ]]; then 
  declare -a THE_TEST=($THE_TEST)
  echo Testing List ${THE_TEST[@]}
fi

function check_known_failures {
  if [[ -f known_failures/$2 ]]; then 
    let invalid_known_fail=$invalid_known_fail+1;
    echo "TEST PASS -- KNOWN FAIL FILE EXISTS"
  fi
}

function set_fail {
  diff $1 known_failures/$2 &> /dev/null
  if [[ $? == 0 ]]; then 
    echo "known failure "
    let known_fail=$known_fail+1;
  else 
    let fail=$fail+1;
    local base=`basename $1`;
    if [ -f known_failures/$2 ]; then  # should have been a failure
      echo "KNOWN FAILURE -- FAILED "
      echo $base > ../build/test/known_failure_diffs/$base
      diff $1 known_failures/$2 >> ../build/test/known_failure_diffs/$base 2>&1
      cp known_failures/$2 ../build/test/html/$base.known_failure.html
    else
      echo "FAILED "
    fi

    echo $base > ../build/test/diffs/$base
    diff $1 golden_results/$2 | cat -t >> ../build/test/diffs/$base 2>&1
    cp $1 ../build/test/html/$base.html
    if [ -f golden_results/$2 ]; then 
      cp golden_results/$2 ../build/test/html/$base.golden.html
    fi
  fi 
}

function run_diff {
  let runs=$runs+1;
  ran=1
  if [[ $NO_LINE == true ]]; then 
    if [[ -f golden_results/$2 ]]; then 
      if [[ $changed == true ]]; then
        diff $1 golden_results/$2 > /dev/null
        if [[ $? == 0 ]]; then # pass
          run_replace=false;
        fi
      fi
    fi
  fi

  diff $1 golden_results/$2 > /dev/null
  
  if [[ $? == 0 ]]; then # pass
    check_known_failures $1 $2
  else 
    set_fail $1 $2
  fi
}

function run_phantomjs {
  local file=$1;
  local tmp=phantomjs_temp_file.html
  local path=test/files_for_test/phantomjs
  cp $file ../$tmp
  # Placeholder for any necessary path transformations
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
  local file=../build/test/results/$test_file;

  echo "testing $base";
  
  local command=("./splunk_capture" "$options")

  echo "${command[@]}"; 

  ( cd $TEST_PATH; eval "${command[@]}" ) &> $file
  if [[ -n $3 ]]; then # call callback
    $3 >> $file 
  fi
  if [[ -n $PHANTOMJS ]]; then
    run_phantomjs $file
  fi
  run_diff $file $test_file;
  return 0;
}

function start {
  let time_start1=`date +"%s"`;
  let fail=0;
  let known_fail=0;
  let invalid_known_fail=0;
  let runs=0;

  export TEST_PATH=".."

  if [[ $invalid_known_fail -ne 0 ]]; then 
    echo "# invalid known failure files " $invalid_known_fail; 
  fi;

  if [[ $fail -ne 0 || $known_fail -ne 0 ]]; then 
    echo "# of unexpected failures " $fail; 
    echo "# of known failures " $known_fail;
  fi;

  # run test against golden results
  # there should be the same number of test files as there are golden results
  let count=0; 
  for i in `ls golden_results/*`; 
  	do 
  		let count=$count+1;
  		local file_name=`echo $i | sed 's:.*/::'`;
  		local test_options=`cat tests/$file_name`
  		run "$test_options" $file_name; 
  done

  let time_start2=`date +"%s"`;
  let time_start1=$time_start2-$time_start1;
  echo "seconds $time_start1"

  echo "# of runs " $runs;

  if [ $runs -ne $count ]; then 
    let x=$count-$runs; 
    echo "NOT RUN $x";
  fi
  date
}

if [[ ! -d ../build/test/results ]]; then
  mkdir -p ../build/test/results;
fi
if [[ ! -d ../build/test/diffs ]]; then
  mkdir -p ../build/test/diffs;
fi
if [[ ! -d ../build/test/html/ ]]; then
  mkdir -p ../build/test/html/
fi
if [[ ! -d ../build/test/known_failure_diffs ]]; then
  mkdir -p ../build/test/known_failure_diffs;
fi

if [[ -z $NO_RUN ]];  then 
  rm -f  ../build/test/results/*

  start;
fi
