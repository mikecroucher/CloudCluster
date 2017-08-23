# CloudCluster

Log in instructions will be given to you in class. 

# Submit a pre-prepared job

Once you have logged in, here's how to submit your first job quickly 

Clone this repository
```
git clone https://github.com/mikecroucher/CloudCluster
```

Navigate to the directory containing the example
```
cd CloudCluster/MPI_hello_world/
```

Submit it
```
qsub submit_mpi.sh
```

See the status of your jobs
```
qstat
```

See the status of all jobs, from all users, running and waiting on the cluster

```
qstat -u '*'
```
