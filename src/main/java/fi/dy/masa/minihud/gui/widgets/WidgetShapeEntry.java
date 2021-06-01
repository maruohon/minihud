package fi.dy.masa.minihud.gui.widgets;

import java.util.List;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.util.math.MatrixStack;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ButtonOnOff;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.widgets.WidgetListEntryBase;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class WidgetShapeEntry extends WidgetListEntryBase<ShapeBase>
{
    private final WidgetListShapes parent;
    private final ShapeBase shape;
    private final List<String> hoverLines;
    private final boolean isOdd;
    private final int buttonsStartX;

    public WidgetShapeEntry(int x, int y, int width, int height, boolean isOdd,
            ShapeBase shape, int listIndex, WidgetListShapes parent)
    {
        super(x, y, width, height, shape, listIndex);

        this.shape = shape;
        this.hoverLines = shape.getWidgetHoverLines();
        this.isOdd = isOdd;
        this.parent = parent;
        y += 1;

        int posX = x + width - 2;

        posX -= this.addButton(posX, y, ButtonListener.Type.REMOVE);
        posX -= this.createButtonOnOff(posX, y, this.shape.isEnabled(), ButtonListener.Type.ENABLED);
        posX -= this.addButton(posX, y, ButtonListener.Type.CONFIGURE);

        this.buttonsStartX = posX;
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
    public void render(int mouseX, int mouseY, boolean selected, MatrixStack matrixStack)
    {
        RenderUtils.color(1f, 1f, 1f, 1f);

        boolean shapeSelected = ShapeManager.INSTANCE.getSelectedShape() == this.entry;

        // Draw a lighter background for the hovered and the selected entry
        if (selected || shapeSelected || this.isMouseOver(mouseX, mouseY))
        {
            RenderUtils.drawRect(this.x, this.y, this.width, this.height, 0x70FFFFFF);
        }
        else if (this.isOdd)
        {
            RenderUtils.drawRect(this.x, this.y, this.width, this.height, 0x20FFFFFF);
        }
        // Draw a slightly lighter background for even entries
        else
        {
            RenderUtils.drawRect(this.x, this.y, this.width, this.height, 0x50FFFFFF);
        }

        if (shapeSelected)
        {
            RenderUtils.drawOutline(this.x, this.y, this.width, this.height, 0xFFE0E0E0);
        }

        String name = this.shape.getDisplayName();
        this.drawString(this.x + 4, this.y + 7, 0xFFFFFFFF, name, matrixStack);

        RenderUtils.color(1f, 1f, 1f, 1f);
        RenderSystem.disableBlend();

        super.render(mouseX, mouseY, selected, matrixStack);

        RenderUtils.disableDiffuseLighting();
    }

    @Override
    public void postRenderHovered(int mouseX, int mouseY, boolean selected, MatrixStack matrixStack)
    {
        super.postRenderHovered(mouseX, mouseY, selected, matrixStack);

        if (mouseX >= this.x && mouseX < this.buttonsStartX && mouseY >= this.y && mouseY <= this.y + this.height)
        {
            RenderUtils.drawHoverText(mouseX, mouseY, this.hoverLines, matrixStack);
        }
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
