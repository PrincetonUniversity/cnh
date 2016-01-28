#!/bin/bash
cd ../Data/VMS
for f in VMS*
    do
        echo "Processing $f…"
        cp $f test
        
        linenumber=$(grep -nm1 '\---' test | cut -f1 -d:)

        # length of fields
        first=$(sed -n "${linenumber}p" test | awk '{ print length($1);}')
        second=$(sed -n "${linenumber}p" test | awk  '{ print length($2);}')
        second=`expr $second + 1`
        third=$(sed -n "${linenumber}p" test | awk  '{ print length($3);}')
        third=`expr $third + 1`
        fourth=$(sed -n "${linenumber}p" test | awk  '{ print length($4);}')
        fourth=`expr $fourth + 1`
        fifth=$(sed -n "${linenumber}p" test | awk  '{ print length($5);}')
        fifth=`expr $fifth + 1`
        sixth=$(sed -n "${linenumber}p" test | awk  '{ print length($6);}')
        sixth=`expr $sixth + 1`
        seventh=$(sed -n "${linenumber}p" test | awk  '{ print length($7);}')
        seventh=`expr $seventh + 1`
        eighth=$(sed -n "${linenumber}p" test | awk  '{ print length($8);}')
        eighth=`expr $eighth + 1`
        
        awk -v FIELDWIDTHS="$first $second $third $fourth $fifth $sixth $seventh $eighth" -v OFS=',' '{$1=$1 ""; print }'  test >  try
        grep -v '^VESSEL NAME\|---\|NAME ' try > temp
        sed '/.\{2\}/!d'  temp > stripped
        perl -i -pe 's/\xB0/,/g' stripped
        perl -p -i -e 's/ +/ /g' stripped
        awk -F, 'BEGIN{OFS=","}{gsub("[.]","-",$7)}1' stripped > dashed
        sfx=$(echo "$f")
        sed "s/$/ ,$sfx/" dashed  > named
        cp named done_$f
    done

    cat done* > VMS.txt
    mv VMS.txt /wrk2/efuller/CNH/VMS_cleaning/data
    rm -f  done*
    rm -f temp stripped try dashed named test 
