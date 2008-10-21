Executable = --EXEC--
Arguments = --ARGS--
Universe = vanilla
Getenv = True
Requirements = ((OpSys == "LINUX")  && (Arch == "X86_64") && (Machine > "lccd") && (Machine < "lcce"))

should_transfer_files = YES
when_to_transfer_output = ON_EXIT
transfer_input_files = --FILES--
notification = NEVER

Log = condor.ff.--ARGS--.log
Output = condor.ff.--ARGS--.out
Error = condor.ff.--ARGS--.err

DiskUsage = 150000


