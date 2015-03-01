#!/bin/bash

curl -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X POST -d '[{"name": "HelloWorld.java", "content": "public class HelloWorld { \n   public static void main(String[] args) { \n      System.out.println(\"Hello, World\");\n   }\n}\n"}]' localhost:8090/languages/java/latest/run | jq .
