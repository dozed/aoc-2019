using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using System.Threading.Channels;
using Priority_Queue;

namespace AdventOfCodeCS
{

    class Node
    {
        public Pos pos;
        public Tile tile;

        public Node(Pos pos, Tile tile)
        {
            this.pos = pos;
            this.tile = tile;
        }

        public override string ToString()
        {
            return $"Node pos={pos} tile={tile}";
        }

        protected bool Equals(Node other)
        {
            return Equals(pos, other.pos);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((Node) obj);
        }

        public override int GetHashCode()
        {
            return (pos != null ? pos.GetHashCode() : 0);
        }
    }
    

    enum Dir
    {
        Up, Down, Left, Right
    }

    class Pos
    {
        public int X { get; set; }
        public int Y { get; set; }

        public Pos(int x, int y)
        {
            X = x;
            Y = y;
        }

        public override string ToString()
        {
            return $"Pos X: {X} Y: {Y}";
        }

        protected bool Equals(Pos other)
        {
            return X == other.X && Y == other.Y;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((Pos) obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(X, Y);
        }
    }

    interface Tile
    {
        
        static Tile GetTile(char c)
        {
            if (c == '#')
            {
                return new Wall();
            }
            else if (c == '.')
            {
                return new Empty();
            }
            else if (c == '@')
            {
                return new Gate();
            }
            else if (Char.IsLower(c))
            {
                return new Key(c);
            }
            else
            {
                return new Door(c);
            } 
        }

        static bool IsGate(Tile t)
        {
            if (t is Gate)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        
        static bool IsWall(Tile t)
        {
            if (t is Wall)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        
        static bool IsKey(Tile t)
        {
            if (t is Key)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        
        static bool IsDoor(Tile t)
        {
            if (t is Door)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        static bool IsInteresting(Tile t)
        {
            // return IsGate(t) || IsDoor(t) || IsKey(t);
            return IsGate(t) || IsKey(t);
        }
        
        static char? GetChar(Tile t)
        {
            if (t is Key)
            {
                return ((Key) t).value;
            }
            else if (t is Gate)
            {
                return '@';
            }
            else
            {
                return null;
            }
        }
        
        static char? GetKey(Tile t)
        {
            if (t is Key)
            {
                return ((Key) t).value;
            }
            else
            {
                return null;
            }
        }
        
        static List<char> GetKeys(Tile t)
        {
            if (t is Key)
            {
                return new List<char>(new [] { ((Key) t).value });
            }
            else
            {
                return new List<char>();
            }
        }
        
        static List<char> GetDoors(Tile t)
        {
            if (t is Door)
            {
                return new List<char>(new [] { ((Door) t).value });
            }
            else
            {
                return new List<char>();
            }
        }
        
    }

    class Wall : Tile
    {
        public override string ToString()
        {
            return "Wall";
        }
    }

    class Empty : Tile
    {
        public override string ToString()
        {
            return "Empty";
        }
    }

    class Key : Tile
    {
        public char value;
        public Key(char v)
        {
            value = v;
        }

        public override string ToString()
        {
            return $"Key {value}";
        }
    } 

    class Door : Tile
    {
        public char value;
        public Door(char v)
        {
            value = v;
        }

        public override string ToString()
        {
            return $"Door {value}";
        }
    }

    class Gate : Tile
    {
        public override string ToString()
        {
            return $"Gate";
        }
    }
    
    class TupleComparer : IEqualityComparer<(char, HashSet<char>)>
    {
        IEqualityComparer<HashSet<char>> comparer = HashSet<char>.CreateSetComparer();

        public bool Equals((char, HashSet<char>) x, (char, HashSet<char>) y)
        {
            return char.Equals(x.Item1, y.Item1) && comparer.Equals(x.Item2, y.Item2);
        }

        public int GetHashCode((char, HashSet<char>) obj)
        {
            return obj.Item1.GetHashCode() + comparer.GetHashCode(obj.Item2);
        }
    }

    class Exercise18
    {
        public static void Exercise18a()
        {

            Pos GetGate(Dictionary<Pos, Tile> map)
            {
                return map.First(x => Tile.IsGate(x.Value)).Key;
            }

            Pos AddDirToPos(Pos pos, Dir dir)
            {
                if (dir == Dir.Up)
                {
                    return new Pos(pos.X, pos.Y - 1);
                }
                else if (dir == Dir.Down)
                {
                    return new Pos(pos.X, pos.Y + 1);
                }
                else if (dir == Dir.Left)
                {
                    return new Pos(pos.X - 1, pos.Y);
                }
                else
                {
                    return new Pos(pos.X + 1, pos.Y);
                }
            }
            
            Dictionary<Pos, Tile> MkMap(string filePath)
            {
                var lines = System.IO.File.ReadLines(filePath);

                var height = lines.Count();
                var width = lines.First().Length;
                // Console.WriteLine($"height {height} - width {width}");

                var map = new Dictionary<Pos, Tile>();

                foreach (var y in Enumerable.Range(0, height-1))
                {
                    foreach (var x in Enumerable.Range(0, width-1))
                    {
                        var c = lines.ElementAt(y).ElementAt(x);
                        Tile t = Tile.GetTile(c);
                        map.Add(new Pos(x, y), t);
                    }
                }

                return map;
            }

            void AddEdge(Dictionary<Node, HashSet<Node>> graph, Node u, Node v)
            {
                if (graph.ContainsKey(u))
                {
                    var adj = graph.GetValueOrDefault(u, new HashSet<Node>());
                    adj.Add(v);
                }
                else
                {
                    var adj = new HashSet<Node>();
                    adj.Add(v);
                    graph.Add(u, adj);
                }
            }

            Dictionary<Node, HashSet<Node>> MkGraph(Dictionary<Pos, Tile> map, Dictionary<Node, HashSet<Node>> graph, HashSet<Node> visited, Queue<Node> toVisit)
            {
                if (toVisit.Count == 0)
                {
                    return graph;
                }
                else
                {
                    var x = toVisit.Dequeue();

                    // Console.WriteLine(x);
            
                    var directions = new Dir[] { Dir.Up, Dir.Down, Dir.Left, Dir.Right };
                    
                    foreach (var dir in directions)
                    {
                        var nextPos = AddDirToPos(x.pos, dir);
                        var nextTile = map.GetValueOrDefault(nextPos, new Wall());

                        if (!Tile.IsWall(nextTile))
                        {
                            var nextNode = new Node(nextPos, nextTile);

                            if (!visited.Contains(nextNode) && !toVisit.Contains(nextNode))
                            {
                                toVisit.Enqueue(nextNode);
                            }

                            // predecessor
                            AddEdge(graph, x, nextNode);
                            AddEdge(graph, nextNode, x);
                        }
                    }

                    visited.Add(x);

                    return MkGraph(map, graph, visited, toVisit);
                }
            }

            Node FindNodeByKey(Dictionary<Node, HashSet<Node>> graph, char key)
            {
                return graph.Keys.First(n => Tile.GetKey(n.tile) == key);
            }

            List<char> GetKeys(List<Node> route)
            {
                return route.SelectMany(n => Tile.GetKeys(n.tile)).OrderBy(x => x).ToList();
            }

            List<char> GetDoors(List<Node> route)
            {
                return route.SelectMany(n => Tile.GetDoors(n.tile)).OrderBy(x => x).ToList();
            }

            List<char> FindKeys(Dictionary<Node, HashSet<Node>> graph)
            {
                return graph.Keys.SelectMany(n => Tile.GetKeys(n.tile)).OrderBy(x => x).ToList();
            }

            Node FindGateNode(Dictionary<Node, HashSet<Node>> graph)
            {
                return graph.Keys.First(n => Tile.IsGate(n.tile));
            }

            // single source shortest paths
            void FindPaths(Dictionary<Node, HashSet<Node>> graph, HashSet<Node> visited, List<Node> toVisit, Dictionary<Node, Node> predecessor)
            {
                while (toVisit.Count > 0)
                {
                    Node u = toVisit.First();
                    toVisit.RemoveAt(0);
                    // Console.WriteLine(u);

                    var neighborsToVisit =
                        graph.GetValueOrDefault(u, new HashSet<Node>())
                            .Where(v => !visited.Contains(v) && !toVisit.Contains(v))
                            .ToList();

                    toVisit.AddRange(neighborsToVisit);

                    visited.Add(u);
                
                    foreach (var v in neighborsToVisit)
                    {
                        predecessor.Add(v, u);
                    }
                    
                    FindPaths(graph, visited, toVisit, predecessor);
                }
            }
            
            // find path from u to v given predecessors map
            List<Node> FindPath(Dictionary<Node, Node> predecessors, Node u, Node v, List<Node> path)
            {
                path.Add(v);

                var p = predecessors[v];

                if (Equals(p, u))
                {
                    path.Add(u);
                    path.Reverse();
                    return path;
                }
                else
                {
                    return FindPath(predecessors, u, p, path);
                }
            }
            
            // all sources shortest paths
            Dictionary<Node, Dictionary<Node, (int, HashSet<char>)>> PrecomputeShortestPaths(Dictionary<Node, HashSet<Node>> graph)
            {
                var xs = new Dictionary<Node, Dictionary<Node, (int, HashSet<char>)>>();

                foreach (var u in graph.Keys)
                {
                    if (!Tile.IsInteresting(u.tile)) continue;
                    
                    var ys = new Dictionary<Node, (int, HashSet<char>)>();

                    var visited = new HashSet<Node>();
                    var toVisit = new List<Node>();
                    toVisit.Add(u);
                    
                    var pred = new Dictionary<Node, Node>();

                    FindPaths(graph, visited, toVisit, pred);

                    foreach (var v in graph.Keys)
                    {
                        if (Equals(u, v)) continue;
                        if (!Tile.IsInteresting(v.tile)) continue;
                        
                        var path = FindPath(pred, u, v, new List<Node>());
                        var keysRequired = GetDoors(path).Select(char.ToLower).ToHashSet();
                        var routeLen = path.Count - 1;
                        
                        ys.Add(v, (routeLen, keysRequired));
                    }

                    xs.Add(u, ys);
                }

                return xs;
            }
            
            var map = MkMap("input/input18.txt");

            var gatePos = GetGate(map);
            Console.WriteLine($"gate at {gatePos}");

            var gateNode = new Node(gatePos, new Gate());

            var graph = MkGraph(map, new Dictionary<Node, HashSet<Node>>(), new HashSet<Node>(), new Queue<Node>(new []{gateNode}));
            Console.WriteLine(graph);

            Console.WriteLine("computing predecessors");
            // var pred = PrecomputeShortestPath(graph);
            var sp = PrecomputeShortestPaths(graph);
            Console.WriteLine($"done, computed {sp.Count} x {sp[gateNode].Count} table of routes");

            var allKeys = FindKeys(graph);
            Console.WriteLine($"keys: {string.Join(',', allKeys)}");

            var Q = new SimplePriorityQueue<(Node, char, int, HashSet<char>), int>();
            Q.Enqueue((gateNode, '@', 0, new HashSet<char>()), 0);

            var seen = new HashSet<(char, HashSet<char>)>(new TupleComparer());
            
            while (Q.Count > 0)
            {
                var (node, key, distance, keys) = Q.Dequeue();
                Console.WriteLine($"{string.Join(',', keys)} {distance}");

                // ignore longer or same paths which have the same keys at one key position
                if (seen.Contains((key, keys))) continue;
                seen.Add((key, keys));
                
                if (keys.Count == allKeys.Count)
                {
                    Console.WriteLine($"distance {distance}");
                    break;
                }
                
                foreach (var nextKey in allKeys)
                {
                    // prevent loops
                    if (keys.Contains(nextKey)) continue;

                    var keyNode = FindNodeByKey(graph, nextKey);
                    var (routeLen, routeKeysRequired) = sp[node][keyNode];

                    if (keys.IsSupersetOf(routeKeysRequired))
                    {
                        var newKeys = new HashSet<char>(keys);
                        newKeys.Add(nextKey);

                        var newDistance = distance + routeLen;

                        Q.Enqueue((keyNode, nextKey, newDistance, newKeys), newDistance);
                    }
                }
            }
        }
    }
}