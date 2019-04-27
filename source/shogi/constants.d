module shogi.constants;

enum Teban { SENTE = 0, GOTE = -1 };

const string[14] KOMA = [ "FU", "KY", "KE", "GI", "KA", "HI", "KI", "OU", "pFU", "pKY", "pKE", "pGI", "pKA", "pHI" ];
const string[10] KOMA_BB = [ "FU", "KY", "KE", "GI", "KA", "HI", "KI", "OU", "pKA", "pHI" ];

mixin({
  import std.format, std.algorithm, std.array;
  string s1 = "enum komaType{none=0,";
  string s2 = "enum komaTypeWP{none=0,";
  foreach (i, k; KOMA.dup.map !(a => [ "B" ~a, "W" ~a ]).join) {
    s1 ~= format("%s=%d,", k, i + 4);
    s2 ~= format("%s=cast(int)komaType.%s<<1,", k, k);
    if (i < 12) s2 ~= format("%sp=cast(int)%s+1,", k, k);
  }
  return s1 ~"};" ~s2 ~"};";
}());

string generateReplace(string qs, string target, in string[] list) {
  import std.algorithm, std.string;
  return list.map !(a => qs.replace(target, a)).join;
}
string generateReplace(string qs, string target1, string target2, in string[2] list) {
  import std.algorithm, std.string, std.range;
  return list.array.permutations.map !(a => qs.replace(target1, a[0]).replace(target2, a[1])).join;
}
unittest {
  assert("TestXX".generateReplace("XX", [ "aaa", "bbb", "ccc" ]) == "TestaaaTestbbbTestccc");
  assert("TestYYZZ".generateReplace("YY", "ZZ", [ "B", "W" ]) == "TestBWTestWB");
}
