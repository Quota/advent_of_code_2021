import java.util.*;
import java.util.stream.*;
import java.nio.file.*;

// Usage: java Day12.java <input_file>
public class Day12 {
  public static void main(String[] args) throws Exception {
    // build neighbors map
    var neighbors = buildNeighbors(Files.readAllLines(Path.of(args[0])), "start", "end");

    // calculate paths
    var result = calculatePaths(neighbors, "start", "end");

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

  static List<List<String>> calculatePaths(Map<String, List<String>> neighbors, String curr, String end) {
    return calculatePaths(0, neighbors, new HashSet<>(), curr, end, List.of("start"));
  }

  static List<List<String>> calculatePaths(int level, Map<String, List<String>> neighbors, Set<String> visited, String curr, String end, List<String> path) {
    // debug output
    for(int i = 0; i < level; i++)
      log("  ");
    log(curr);

    // check end cave
    if(end.equals(curr)) {
      log(" -> " + path + "\n");
      return List.of(path);
    }

    // check/remember lower case caves
    if(visited.contains(curr)) {
      log(" (already been)\n");
      return null;
    }
    final Set<String> vis2;
    if(Character.isLowerCase(curr.charAt(0))) {
      vis2 = new HashSet<>(visited);
      vis2.add(curr);
    } else {
      vis2 = visited;
    }

    // recur to neighbor caves
    log("\n");
    return neighbors.getOrDefault(curr, Collections.emptyList())
                    .stream()
                    .map(neigh -> copyAndAdd(path, neigh))
                    .map(neighPath -> calculatePaths(level+1, neighbors, vis2, neighPath.get(neighPath.size()-1), end, neighPath))
                    .filter(Objects::nonNull)
                    .flatMap(Collection::stream)
                    .collect(Collectors.toList());
  }

  // helper to create a copy of `in`, add `item` to it and return it
  static List<String> copyAndAdd(List<String> in, String item)
  {
    var out = new ArrayList<>(in);
    out.add(item);
    return out;
  }
}
