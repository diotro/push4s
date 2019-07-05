package object push4s {
  // A Push Program is simply a sequence of Push atoms. These atoms will all be pushed onto the
  // exec stack at the beginning of runtime.
  type PushProgram = Seq[PushAtom]
}
