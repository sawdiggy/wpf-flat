using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using Froto.Roslyn;

namespace Froto.Gen
{
    class RecursiveGenerator
    {
        static void Main(string[] args)
        {
            if (args.Length < 2)
                throw new Exception("Args: (1) protoRoot, (2) output path, (3-n) ignore protos");

            string root = args[0];
            string output = args[1];

            var excludeList = args.Length > 2 ? args[2].Split(';') : new List<string>().ToArray();

            var list = new string[excludeList.Length];

            for (var i = 0; i < excludeList.Length; i++)
                list[i] = excludeList[i];

            Console.WriteLine("rootPath {0}, outputPath {1}", root, output);

            foreach (var exclude in list)
                Console.WriteLine("exclude path {0}\n", exclude);

            ProtoGen.generate(root, output, list);
        }
    }
}
