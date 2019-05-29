package fi.dy.masa.minihud.gui.widgets;

import java.util.Collection;
import javax.annotation.Nullable;
import fi.dy.masa.malilib.gui.interfaces.ISelectionListener;
import fi.dy.masa.malilib.gui.widgets.WidgetListBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class WidgetListShapes extends WidgetListBase<ShapeBase, WidgetShapeEntry>
{
    public WidgetListShapes(int x, int y, int width, int height, float zLevel,
            @Nullable ISelectionListener<ShapeBase> selectionListener)
    {
        super(x, y, width, height, selectionListener);

        this.browserEntryHeight = 22;
    }

    @Override
    protected Collection<ShapeBase> getAllEntries()
    {
        return ShapeManager.INSTANCE.getAllShapes();
    }

    @Override
    protected WidgetShapeEntry createListEntryWidget(int x, int y, int listIndex, boolean isOdd, ShapeBase entry)
    {
        return new WidgetShapeEntry(x, y, this.browserEntryWidth,
                this.getBrowserEntryHeightFor(entry), isOdd, entry, listIndex, this);
    }
}
