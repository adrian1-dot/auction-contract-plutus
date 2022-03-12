
GivenTime=$(expr $1 \* 60)
CurrentTime=$(date +%s)
Deadline=`expr $(expr $GivenTime + $CurrentTime) \* 1000`
echo $Deadline > $1-min.time


