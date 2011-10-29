package ideah.tree;

import com.intellij.openapi.util.TextRange;
import ideah.util.LineCol;
import ideah.util.LineColRange;

import java.io.FileReader;
import java.io.IOException;

public final class TreeDebug {

    private static final class MyLocation implements ILocation {

        private final LineCol coord;

        private MyLocation(LineCol coord) {
            this.coord = coord;
        }

        public int compareTo(ILocation o) {
            MyLocation that = (MyLocation) o;
            return this.coord.compareTo(that.coord);
        }
    }

    private static final class MyRange implements IRange {

        private final ILocation start;
        private final ILocation end;

        private MyRange(LineColRange range) {
            this.start = new MyLocation(range.start);
            this.end = new MyLocation(range.end);
        }

        private MyRange(ILocation start, ILocation end) {
            this.start = start;
            this.end = end;
        }

        public TextRange getRange() {
            return null;
        }

        public ILocation getStart() {
            return start;
        }

        public ILocation getEnd() {
            return end;
        }
    }

    public static void main(String[] args) throws IOException, NoMatchException {
        TreeParser parser = new TreeParser(new FileReader("C:\\work\\projects\\ideah\\src\\haskell\\ask_ghc\\err"), new RangeFactory() {

            public IRange parse(String str) {
                LineColRange range = new LineColRange(str);
                return new MyRange(range);
            }

            public IRange create(ILocation start, ILocation end) {
                return new MyRange(start, end);
            }
        });
        ModuleTree module = parser.readTree(new MyRange(LineColRange.getFake()));
        System.out.println(module);
    }
}
