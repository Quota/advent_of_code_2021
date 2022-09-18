import java.util.*;
import java.util.stream.*;
import java.nio.file.*;

// Usage: java Day12.java <part:1|2> <input_file>
public class Day12 {
  public static void main(String[] args) throws Exception {
    var lines = Files.readAllLines(Path.of(args[1]));
    var timer = new Timer();
    timer.start();
    if("1".equals(args[0]))
      part1(lines);
    else
      part2(lines);
    timer.stop("Total");
  }

  static void part1(List<String> lines) throws Exception {
    Timer timer = new Timer();
    // build neighbors map
    timer.start();
    var neighbors = buildNeighbors(lines, "start", "end");
    timer.stop("Neighbors");

    // calculate paths
    timer.start();
    var result = calculatePaths(neighbors, "start", "end", new SingleVisitRule());
    timer.stop("Calc");

    // print result
    System.out.println("Neighbor map: " + neighbors);
    System.out.println("Paths: " + result.size());
    //result.forEach(System.out::println);
  }

  static void part2(List<String> lines) throws Exception {
    var timer = new Timer();
    // build neighbors map
    timer.start();
    var neighbors = buildNeighbors(lines, "start", "end");
    timer.stop("Neighbors");

    // find all small caves (except start/end)
    timer.start();
    var smallCaves = neighbors.keySet()
                              .stream()
                              .filter(c -> Character.isLowerCase(c.charAt(0)))
                              .filter(c -> !"start".equals(c) && !"end".equals(c))
                              .collect(Collectors.toSet());
    timer.stop("Small caves");

    // calculate paths
    var result = new HashSet<List<String>>();
    for(String smallCave: smallCaves) {
      log("TWICE allowed: " + smallCave + "\n");

      timer.start();
      result.addAll(calculatePaths(neighbors, "start", "end", new OneTwiceVisitRule(smallCave)));
      timer.stop(smallCave + " twice");
    }

    // print result
    System.out.println("Neighbor map: " + neighbors);
    System.out.println("Paths: " + result.size());
    //result.forEach(System.out::println);
  }

  static void log(String str) {
    //System.err.print(str);
  }

  static Map<String, List<String>> buildNeighbors(List<String> lines, String start, String end) {
    var res = new HashMap<String, List<String>>();
    for(var line: lines) {
      String[] leftRight = line.split("-");
      if(!leftRight[0].equals(end) && !leftRight[1].equals(start))
        res.computeIfAbsent(leftRight[0], key -> new ArrayList<String>()).add(leftRight[1]);
      if(!leftRight[1].equals(end) && !leftRight[0].equals(start))
        res.computeIfAbsent(leftRight[1], key -> new ArrayList<String>()).add(leftRight[0]);
    }
    return res;
  }

  static Set<List<String>> calculatePaths(Map<String, List<String>> neighbors, String curr, String end, VisitRule visitRule) {
    return calculatePaths(0, neighbors, visitRule, curr, end, List.of("start"));
  }

  static Set<List<String>> calculatePaths(int level, Map<String, List<String>> neighbors, VisitRule visitRule, String curr, String end, List<String> path) {
    // debug output
    for(int i = 0; i < level; i++)
      log("  ");
    log(curr);

    // check end cave
    if(end.equals(curr)) {
      log(" -> " + path + "\n");
      return Set.of(path);
    }

    // check/remember lower case caves
    VisitRule nextVisitRule = visitRule.visit(curr);
    if(nextVisitRule == null) {
      log(" (already been)\n");
      return null;
    }

    // recur to neighbor caves
    log("\n");
    return neighbors.getOrDefault(curr, Collections.emptyList())
                    .stream()
                    .map(neigh -> copyAndAdd(path, neigh))
                    .map(neighPath -> calculatePaths(level+1, neighbors, nextVisitRule, neighPath.get(neighPath.size()-1), end, neighPath))
                    .filter(Objects::nonNull)
                    .flatMap(Collection::stream)
                    .collect(Collectors.toSet());
  }

  // helper to create a copy of `in`, add `item` to it and return it
  static List<String> copyAndAdd(List<String> in, String item)
  {
    var out = new ArrayList<>(in);
    out.add(item);
    return out;
  }

  interface VisitRule {
    /** Returns the next VisitRule to continue, or null to abort. */
    VisitRule visit(String value);
  }

  static class SingleVisitRule implements VisitRule {
    Set<String> visited = new HashSet<>();
    public VisitRule visit(String value) {
      if(visited.contains(value))
        return null;
      if(Character.isLowerCase(value.charAt(0))) {
        SingleVisitRule ret = new SingleVisitRule();
        ret.visited.addAll(visited);
        ret.visited.add(value);
        return ret;
      }
      return this;
    }
  }

  static class OneTwiceVisitRule implements VisitRule {
    Set<String> visited = new HashSet<>();
    String twiceValue;
    boolean twiceValueVisited;
    OneTwiceVisitRule(String twiceValue) {
      this.twiceValue = twiceValue;
    }
    OneTwiceVisitRule(OneTwiceVisitRule other) {
      visited.addAll(other.visited);
      twiceValue = other.twiceValue;
      twiceValueVisited = other.twiceValueVisited;
    }
    boolean isVisited(String value) {
      if(twiceValue.equals(value)) {
        return visited.contains(value) && twiceValueVisited;
      }
      return visited.contains(value);
    }
    public VisitRule visit(String value) {
      if(isVisited(value))
        return null;
      if(Character.isLowerCase(value.charAt(0))) {
        OneTwiceVisitRule ret = new OneTwiceVisitRule(this);
        ret.visited.add(value);
        if(twiceValue.equals(value) && visited.contains(value)) // NOT ret.visited.contains(value) !!!
          ret.twiceValueVisited = true;
        return ret;
      }
      return this;
    }
  }

  static class Timer {
    long t;
    void start() {
      t = System.currentTimeMillis();
    }
    void stop(String msg) {
      t = System.currentTimeMillis() - t;
      System.out.println(msg + " time: " + t + " ms");
    }
  }
}
