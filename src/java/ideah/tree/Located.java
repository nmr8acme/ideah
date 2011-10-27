package ideah.tree;

import com.google.common.collect.Iterables;
import ideah.util.ILocation;
import ideah.util.IRange;

import java.util.*;

public abstract class Located {

    public final IRange location;
    private List<Filler> filler = null;

    protected Located(IRange location) {
        this.location = location;
    }

    protected abstract Iterable<? extends Located> getChildren();

    public final void fillGaps(SortedMap<ILocation, Filler> tokens) {
        Iterable<? extends Located> children = getChildren();
        for (Located child : children) {
            child.fillGaps(tokens);
        }
        Filler range = tokens.get(location.getStart());
        if (range != null && range.location.equals(location)) {
            tokens.remove(location.getStart());
        }
        setGaps(tokens);
    }

    private void setGaps(SortedMap<ILocation, Filler> rest) {
        SortedMap<ILocation, Filler> tail = rest.tailMap(location.getStart());
        for (Iterator<Map.Entry<ILocation, Filler>> i = tail.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry<ILocation, Filler> entry = i.next();
            ILocation pos = entry.getKey();
            if (pos.compareTo(location.getEnd()) >= 0)
                break;
            if (filler == null) {
                filler = new ArrayList<Filler>();
            }
            filler.add(entry.getValue());
            i.remove();
        }
    }

    public final List<Located> getBlocks() {
        List<Located> list = new ArrayList<Located>();
        Iterables.addAll(list, getChildren());
        if (filler != null) {
            list.addAll(filler);
        }
        Collections.sort(list, new Comparator<Located>() {
            public int compare(Located o1, Located o2) {
                return o1.location.getStart().compareTo(o2.location.getStart());
            }
        });
        return list;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + " " + location;
    }
}
