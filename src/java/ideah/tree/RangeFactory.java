package ideah.tree;

public interface RangeFactory {

    IRange parse(String str);

    IRange create(ILocation start, ILocation end);
}
