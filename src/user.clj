(ns user ; user ns is loaded by REPL startup
  (:require [hyperfiddle.rcf]))

(hyperfiddle.rcf/enable!)

(println "User Namespace Loaded")