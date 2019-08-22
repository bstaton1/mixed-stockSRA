#!/bin/bash

# delete the temporary files

tmp_files=($(ls cp*))
for tmp_file in ${tmp_files[@]}
do
  rm $tmp_file
done

echo "Temporary Files Removed"