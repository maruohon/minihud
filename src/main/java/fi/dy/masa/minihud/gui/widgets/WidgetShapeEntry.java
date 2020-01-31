package fi.dy.masa.minihud.gui.widgets;

import net.minecraft.client.renderer.GlStateManager;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ButtonOnOff;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.gui.widgets.WidgetListEntryBase;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class WidgetShapeEntry extends WidgetListEntryBase<ShapeBase>
{
    private final WidgetListShapes parent;
    private final ShapeBase shape;
    private final boolean isOdd;
    private final int buttonsStartX;

    public WidgetShapeEntry(int x, int y, int width, int height, boolean isOdd,
            ShapeBase shape, int listIndex, WidgetListShapes parent)
    {
        super(x, y, width, height, shape, listIndex);

        this.shape = shape;
        this.isOdd = isOdd;
        this.parent = parent;
        y += 1;

        int posX = x + width - 2;

        posX -= this.addButton(posX, y, ButtonListener.Type.REMOVE);
        posX -= this.createButtonOnOff(posX, y, this.shape.isEnabled(), ButtonListener.Type.ENABLED);
        posX -= this.addButton(posX, y, ButtonListener.Type.CONFIGURE);

        this.buttonsStartX = posX;

        this.getHoverStrings().addAll(shape.getWidgetHoverLines());
    }

    protected int addButton(int x, int y, ButtonListener.Type type)
    {
        ButtonGeneric button = new ButtonGeneric(x, y, -1, true, type.getDisplayName());
        this.addButton(button, new ButtonListener(type, this));

        return button.getWidth() + 1;
    }

    private int createButtonOnOff(int xRight, int y, boolean isCurrentlyOn, ButtonListener.Type type)
    {
        ButtonOnOff button = new ButtonOnOff(xRight, y, -1, true, type.getTranslationKey(), isCurrentlyOn);
        this.addButton(button, new ButtonListener(type, this));

        return button.getWidth() + 2;
    }

    @Override
    public boolean canSelectAt(int mouseX, int mouseY, int mouseButton)
    {
        return super.canSelectAt(mouseX, mouseY, mouseButton) && mouseX < this.buttonsStartX;
    }

    @Override
    public void render(int mouseX, int mouseY, boolean selected)
    {
        RenderUtils.color(1f, 1f, 1f, 1f);

        boolean shapeSelected = ShapeManager.INSTANCE.getSelectedShape() == this.entry;
        int x = this.getX();
        int y = this.getY();
        int z = this.getZLevel();
        int width = this.getWidth();
        int height = this.getHeight();

        // Draw a lighter background for the hovered and the selected entry
        if (selected || shapeSelected || this.isMouseOver(mouseX, mouseY))
        {
            RenderUtils.drawRect(x, y, width, height, 0x70FFFFFF, z);
        }
        else if (this.isOdd)
        {
            RenderUtils.drawRect(x, y, width, height, 0x20FFFFFF, z);
        }
        // Draw a slightly lighter background for even entries
        else
        {
            RenderUtils.drawRect(x, y, width, height, 0x50FFFFFF, z);
        }

        if (shapeSelected)
        {
            RenderUtils.drawOutline(x, y, width, height, 1, 0xFFE0E0E0, z);
        }

        String name = this.shape.getDisplayName();
        this.drawString(x + 4, y + this.getCenteredTextOffsetY(), 0xFFFFFFFF, name);

        RenderUtils.color(1f, 1f, 1f, 1f);
        GlStateManager.disableBlend();

        super.render(mouseX, mouseY, selected);

        RenderUtils.disableItemLighting();
        GlStateManager.disableLighting();

        RenderUtils.color(1f, 1f, 1f, 1f);
    }

    private static class ButtonListener implements IButtonActionListener
    {
        private final Type type;
        private final WidgetShapeEntry widget;

        public ButtonListener(Type type, WidgetShapeEntry widget)
        {
            this.type = type;
            this.widget = widget;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            if (this.type == Type.CONFIGURE)
            {
                GuiShapeEditor gui = new GuiShapeEditor(this.widget.shape);
                gui.setParent(GuiUtils.getCurrentScreen());
                GuiBase.openGui(gui);
            }
            else if (this.type == Type.ENABLED)
            {
                this.widget.shape.toggleEnabled();
                this.widget.parent.refreshEntries();
            }
            else if (this.type == Type.REMOVE)
            {
                ShapeManager.INSTANCE.removeShape(this.widget.shape);
                this.widget.parent.refreshEntries();
            }
        }

        public enum Type
        {
            CONFIGURE   ("minihud.gui.button.configure"),
            ENABLED     ("minihud.gui.button.shape_entry.enabled"),
            REMOVE      ("minihud.gui.button.remove");

            private final String translationKey;

            private Type(String translationKey)
            {
                this.translationKey = translationKey;
            }

            public String getTranslationKey()
            {
                return this.translationKey;
            }
            
            public String getDisplayName(Object... args)
            {
                return StringUtils.translate(this.translationKey, args);
            }
        }
    }
}
