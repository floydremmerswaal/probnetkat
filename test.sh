#! /bin/bash
# Test all test cases described in .pnk files in the test directory
total=0
correct=0
for OUTPUT in $(ls test/*.pnk); 
do  
	let total=$total+1
	if ~/thesis/probnetkat/parser/Probnetkat/Test $OUTPUT | grep -q 'Parse Successful!'; then
		let correct=$correct+1
		echo "Parsed $OUTPUT"
	else
		echo "FAILED to parse $OUTPUT"
fi
done
echo "Correct $correct / $total"