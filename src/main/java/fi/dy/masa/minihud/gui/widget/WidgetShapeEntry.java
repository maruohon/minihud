package fi.dy.masa.minihud.gui.widget;

import net.minecraft.client.renderer.GlStateManager;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.button.OnOffButton;
import fi.dy.masa.malilib.gui.widget.button.OnOffStyle;
import fi.dy.masa.malilib.gui.widget.list.DataListWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.BaseDataListEntryWidget;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class WidgetShapeEntry extends BaseDataListEntryWidget<ShapeBase>
{
    private final DataListWidget<ShapeBase> parent;
    private final ShapeBase shape;
    private final GenericButton configureButton;
    private final GenericButton toggleButton;
    private final GenericButton removeButton;
    private final int buttonsStartX;

    public WidgetShapeEntry(int x, int y, int width, int height, int listIndex,
                            int originalListIndex, ShapeBase shape, DataListWidget<ShapeBase> parent)
    {
        super(x, y, width, height, listIndex, originalListIndex, shape);

        this.shape = shape;
        this.parent = parent;

        this.configureButton = new GenericButton(x, y + 1, -1, true, "minihud.gui.button.configure");
        this.toggleButton = new OnOffButton(x, y + 1, -1, 20, OnOffStyle.SLIDER_ON_OFF, this.shape::isEnabled, null);
        this.toggleButton.setRightAlign(true, x, true);
        this.removeButton = new GenericButton(x, y + 1, -1, true, "minihud.gui.button.remove");

        this.configureButton.setActionListener((btn, mbtn) -> {
            GuiShapeEditor gui = new GuiShapeEditor(this.shape);
            gui.setParent(GuiUtils.getCurrentScreen());
            BaseScreen.openGui(gui);
        });

        this.toggleButton.setActionListener((btn, mbtn) -> {
            this.shape.toggleEnabled();
            this.parent.refreshEntries();
        });

        this.removeButton.setActionListener((btn, mbtn) -> {
            ShapeManager.INSTANCE.removeShape(this.shape);
            this.parent.refreshEntries();
        });

        this.buttonsStartX = x + width - this.configureButton.getWidth() - this.toggleButton.getWidth() - this.removeButton.getWidth() - 6;

        this.addHoverStrings(shape.getWidgetHoverLines());
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
        return super.canHoverAt(mouseX, mouseY, mouseButton) && mouseX < this.buttonsStartX;
    }

    @Override
    public void renderAt(int x, int y, float z, int mouseX, int mouseY, boolean isActiveGui, boolean hovered)
    {
        RenderUtils.color(1f, 1f, 1f, 1f);

        boolean shapeSelected = ShapeManager.INSTANCE.getSelectedShape() == this.data;
        int width = this.getWidth();
        int height = this.getHeight();

        // Draw a lighter background for the hovered and the selected entry
        if (shapeSelected || hovered)
        {
            RenderUtils.renderRectangle(x, y, width, height, 0x70FFFFFF, z);
        }
        else if (this.isOdd)
        {
            RenderUtils.renderRectangle(x, y, width, height, 0x20FFFFFF, z);
        }
        // Draw a slightly lighter background for even entries
        else
        {
            RenderUtils.renderRectangle(x, y, width, height, 0x50FFFFFF, z);
        }

        if (shapeSelected)
        {
            RenderUtils.renderOutline(x, y, width, height, 1, 0xFFE0E0E0, z);
        }

        String name = this.shape.getDisplayName();
        this.drawString(x + 4, y + this.getCenteredTextOffsetY(), z, 0xFFFFFFFF, name);

        RenderUtils.color(1f, 1f, 1f, 1f);
        GlStateManager.disableBlend();

        super.renderAt(x, y, z, mouseX, mouseY, isActiveGui, hovered);

        RenderUtils.disableItemLighting();
        GlStateManager.disableLighting();

        RenderUtils.color(1f, 1f, 1f, 1f);
    }
}
