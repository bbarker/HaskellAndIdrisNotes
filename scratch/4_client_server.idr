module Main
-- %default total
main: IO ()


process: Int -> Int
process req = req+1
server: Stream Int -> Stream Int
server (req :: reqs)            = process req :: server reqs
client: Int -> Lazy (Stream Int) -> Stream Int
client initreq resps = initreq :: client (head resps) (tail resps)
mutual
  reqsOut: Stream Int
  respsOut: Stream Int
  -- This fixes the segfault:
  -- reqsOut  = cycle [1, 2, 3]
  reqsOut  = client 0 respsOut
  respsOut = server reqsOut

main = do
  printLn(take 10 reqsOut)
