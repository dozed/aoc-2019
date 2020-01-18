using System;
using System.Collections.Generic;
using System.Linq;
using Priority_Queue;

// code converted from https://github.com/JasonCannon/advent-of-code-2019/blob/master/day_18/day_18b.py
namespace AdventOfCodeCS
{
    
    class TupleComparer1 : IEqualityComparer<(char, HashSet<char>)>
    {
        IEqualityComparer<HashSet<char>> comparer = HashSet<char>.CreateSetComparer();

        public bool Equals((char, HashSet<char>) x, (char, HashSet<char>) y)
        {
            return char.Equals(x.Item1, y.Item1) && comparer.Equals(x.Item2, y.Item2);
        }

        public int GetHashCode((char, HashSet<char>) obj)
        {
            // return obj.Item1.GetHashCode() + obj.Item2.GetHashCode();
            // return HashCode.Combine(obj.Item1.GetHashCode(), obj.Item2.GetHashCode());
            return HashCode.Combine(obj.Item1.GetHashCode(), comparer.GetHashCode(obj.Item2));
        }
    }

    public class Exercise18c
    {
        public static void exercise18()
        {
            var lines = System.IO.File.ReadLines("input/input18d.txt").ToList();
            var D = new Dictionary<char, (int, int)>();
            var cnt = (int) '0';

            var Y = new int[] {-1, 0, 1, 0};
            var X = new int[] {0, 1, 0, -1};

            foreach (var r in Enumerable.Range(0, lines.Count))
            {
                foreach (var c in Enumerable.Range(0, lines[0].Length))
                {
                    if (char.IsLower(lines[r][c]))
                    {
                        D[lines[r][c]] = (r, c);
                    }
                    else if (lines[r][c] == '@')
                    {
                        D[(char) cnt] = (r, c);
                        cnt += 1;
                    }
                }
            }

            var numKeys = D.Keys.Count - (cnt - (char)'0');

            Dictionary<char, (int, HashSet<char>)> bfs((int, int) p)
            {
                var Q = new Queue<(int, int, int, HashSet<char>)>();
                Q.Enqueue((p.Item1, p.Item2, 0, new HashSet<char>()));
                
                var seen = new HashSet<(int, int)>();
                var K = new Dictionary<char, (int, HashSet<char>)>();

                while (Q.Count > 0)
                {
                    var (y, x, t, D) = Q.Dequeue();

                    if (seen.Contains((y, x))) continue;
                    seen.Add((y, x));
                    
                    if (char.IsLower(lines[y][x]) && (y,x) != p)
                    {
                        K[lines[y][x]] = (t, new HashSet<char>(D));
                    }
                    
                    foreach (var i in Enumerable.Range(0, 4))
                    {
                        var (dy, dx) = (y + Y[i], x + X[i]);

                        if (lines[dy][dx] != '#')
                        {
                            var DD = new HashSet<char>(D);
                            if (char.IsUpper(lines[dy][dx]))
                            {
                                DD.Add(char.ToLower(lines[dy][dx]));
                            }
                            Q.Enqueue((dy, dx, t+1, DD));
                        }
                    }

                }

                return K;
            }
            
            
            var G = new Dictionary<char, Dictionary<char, (int, HashSet<char>)>>();
            foreach (var c in D.Keys)
            {
                G[c] = bfs(D[c]);
            }
            
            var Q = new SimplePriorityQueue<(char, int, HashSet<char>), int>();
            Q.Enqueue(('0', 0, new HashSet<char>()), 0);
            
            var seen = new HashSet<(char, HashSet<char>)>(new TupleComparer1());
            
            while (Q.Count > 0)
            {
                var (key, d, S) = Q.Dequeue();

                if (seen.Contains((key, S)))
                {
                    // there are less items in dist in c# than in python
                    // <char, set char> hashcode was wrong
                    // without key a wrong result is computed
                    // Console.WriteLine($"{string.Join(',', S)} {d}");
                    continue;
                }
                seen.Add((key, S));

                if (S.Count == numKeys)
                {
                    Console.WriteLine(d);
                    break;
                }

                Console.WriteLine($"{string.Join(',', S)} {d}");

                foreach (var (v, (w, T)) in G[key])
                {
                    if (S.IsSupersetOf(T) && !S.Contains(v))
                    {
                        Q.Enqueue((v, d+w, S.Append(v).ToHashSet()), d+w);
                    }
                }
            }

        }
    }
}