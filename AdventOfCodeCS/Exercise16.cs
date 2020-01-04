using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCodeCS
{
    public class Exercise16
    {
        static IEnumerable<int> replicate(int i, int c)
        {
            return Enumerable.Repeat(c, i).ToList();
        }

        static IEnumerable<int> mkPattern(IEnumerable<int> basePattern, int pos)
        {
            return basePattern.SelectMany(x => replicate(pos, x)).ToList();
        }

        static String ToString<T>(IEnumerable<T> xs)
        {
            return string.Join(",", xs);
        }

        static IEnumerable<int> mkStream(IEnumerable<int> xs)
        {
            while (true)
            {
                foreach (var x in xs)
                {
                    yield return x;
                }
            }
        }

        static IEnumerable<int> computeOutput(IEnumerable<int> input, IEnumerable<int>[] patterns)
        {
            var output = Enumerable.Range(1, input.Count()).Select(i =>
            {
                var pattern = patterns[i - 1];
                var sum = Enumerable.Zip(input, pattern).Select((a, idx) => a.First * a.Second).Sum();
                return Math.Abs(sum) % 10;
            });

            return output;
        }

        public static void part1()
        {
            var basePattern = new List<int>() {0, 1, 0, -1};

            var x = System.IO.File.ReadLines("input/input16.txt").First();
            // var x = "12345678";
            // var x = "80871224585914546619083218645595";
            // var x = "19617804207202209144916044189917";
            // 58672132

            var input = Enumerable.ToList(x).Select(x => int.Parse(x.ToString()));
            Console.WriteLine(ToString(input));

            var patterns = Enumerable.Range(1, input.Count())
                .Select(i => mkStream(mkPattern(basePattern, i)).Skip(1).Take(input.Count()).ToList())
                .ToArray();

            foreach (var i in Enumerable.Range(1, 100))
            {
                input = computeOutput(input, patterns).ToList();
            }

            Console.WriteLine(ToString(input));
        }

        static void part2()
        {
                        
            var basePattern = new List<int>() {0, 1, 0, -1};

            // var x = System.IO.File.ReadLines("input/input16.txt").First();
            // var x = "12345678";
            // var x = "80871224585914546619083218645595";
            // var x = "19617804207202209144916044189917";
            // 58672132
            var x = "03036732577212944063491565474664";
            
            var input = Enumerable.ToList(x).Select(x => int.Parse(x.ToString()));
            input = Enumerable.Repeat(input.ToArray(), 1000).SelectMany(x => x).ToList();
            // Console.WriteLine(ToString(res));

            // var offset = 

            var patterns = Enumerable.Range(1, input.Count())
                .Select(i => mkStream(mkPattern(basePattern, i)).Skip(1).Take(input.Count()).ToList())
                .ToArray();
            
            foreach (var i in Enumerable.Range(1, 100))
            {
                Console.WriteLine(i);
                input = computeOutput(input, patterns).ToList();
            }
            
            Console.WriteLine(ToString(input));

        }
        
    }
}