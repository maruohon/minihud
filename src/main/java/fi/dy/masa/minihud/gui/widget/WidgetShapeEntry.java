package fi.dy.masa.minihud.gui.widget;

import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.button.OnOffButton;
import fi.dy.masa.malilib.gui.widget.button.OnOffStyle;
import fi.dy.masa.malilib.gui.widget.list.DataListWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.BaseDataListEntryWidget;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.render.ShapeRenderUtils;
import fi.dy.masa.malilib.render.text.StyledTextLine;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class WidgetShapeEntry extends BaseDataListEntryWidget<ShapeBase>
{
    private final ShapeBase shape;
    private final GenericButton configureButton;
    private final GenericButton toggleButton;
    private final GenericButton removeButton;
    private final StyledTextLine nameText;
    private final int buttonsStartX;

    public WidgetShapeEntry(int x, int y, int width, int height, int listIndex,
                            int originalListIndex, ShapeBase shape, DataListWidget<ShapeBase> listWidget)
    {
        super(x, y, width, height, listIndex, originalListIndex, shape, listWidget);

        this.shape = shape;

        this.nameText = StyledTextLine.of(shape.getDisplayName());
        this.configureButton = new GenericButton(x, y + 1, -1, true, "minihud.gui.button.configure");
        this.toggleButton = new OnOffButton(x, y + 1, -1, 20, OnOffStyle.SLIDER_ON_OFF, this.shape::isEnabled, null);
        this.toggleButton.setRightAlign(true, x, true);
        this.removeButton = new GenericButton(x, y + 1, -1, true, "minihud.gui.button.remove");

        this.configureButton.setActionListener((btn, mbtn) -> {
            GuiShapeEditor gui = new GuiShapeEditor(this.shape);
            gui.setParent(GuiUtils.getCurrentScreen());
            BaseScreen.openScreen(gui);
        });

        this.toggleButton.setActionListener((btn, mbtn) -> {
            this.shape.toggleEnabled();
            this.listWidget.reCreateListEntryWidgets();
        });

        this.removeButton.setActionListener((btn, mbtn) -> {
            ShapeManager.INSTANCE.removeShape(this.shape);
            this.listWidget.refreshEntries();
        });

        this.buttonsStartX = x + width - this.configureButton.getWidth() - this.toggleButton.getWidth() - this.removeButton.getWidth() - 6;

        this.setHoverStringProvider("shape_info", shape::getWidgetHoverLines);
    }

    @Override
    public void updateSubWidgetsToGeometryChanges()
    {
        super.updateSubWidgetsToGeometryChanges();

        int x = this.getX() + this.getWidth() - 2;

        this.removeButton.setRightX(x);
        x = this.removeButton.getX() - 2;

        this.toggleButton.setRightX(x);
        x = this.toggleButton.getX() - 2;

        this.configureButton.setRightX(x);
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        this.addWidget(this.configureButton);
        this.addWidget(this.toggleButton);
        this.addWidget(this.removeButton);
    }

    @Override
    public boolean canHoverAt(int mouseX, int mouseY, int mouseButton)
    {
        return mouseX < this.buttonsStartX && super.canHoverAt(mouseX, mouseY, mouseButton);
    }

    @Override
    public void renderAt(int x, int y, float z, int mouseX, int mouseY, boolean isActiveGui, boolean hovered)
    {
        boolean shapeSelected = ShapeManager.INSTANCE.getSelectedShape() == this.data;
        int width = this.getWidth();
        int height = this.getHeight();

        // Draw a lighter background for the hovered and the selected entry
        // Draw a slightly lighter background for even entries
        int backgroundColor = (shapeSelected || hovered) ? 0x70FFFFFF : (this.isOdd ? 0x20FFFFFF : 0x50FFFFFF);

        RenderUtils.color(1f, 1f, 1f, 1f);
        ShapeRenderUtils.renderRectangle(x, y, z, width, height, backgroundColor);

        if (shapeSelected)
        {
            ShapeRenderUtils.renderOutline(x, y, z, width, height, 1, 0xFFE0E0E0);
        }

        this.renderTextLine(x + 4, y + this.getCenteredTextOffsetY(), z, 0xFFFFFFFF, false, this.nameText);

        super.renderAt(x, y, z, mouseX, mouseY, isActiveGui, hovered);
    }
}
