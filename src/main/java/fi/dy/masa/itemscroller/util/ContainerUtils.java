package fi.dy.masa.itemscroller.util;

import java.lang.reflect.Field;
import fi.dy.masa.itemscroller.ItemScroller;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Slot;

public class ContainerUtils
{
    private static final Field field_GuiContainer_hoveredSlot = ReflectionHelper.findField(GuiContainer.class, "field_147006_u", "hoveredSlot");
    private static final Field field_GuiContainer_guiLeft = ReflectionHelper.findField(GuiContainer.class, "field_147003_i", "guiLeft");
    private static final Field field_GuiContainer_guiTop = ReflectionHelper.findField(GuiContainer.class, "field_147009_r", "guiTop");
    private static final Field field_GuiContainer_xSize = ReflectionHelper.findField(GuiContainer.class, "field_146999_f", "xSize");
    private static final Field field_GuiContainer_ySize = ReflectionHelper.findField(GuiContainer.class, "field_147000_g", "ySize");

    public static Slot getSlotUnderMouse(GuiContainer gui)
    {
        Slot slot = null;

        try
        {
            slot = (Slot) field_GuiContainer_hoveredSlot.get(gui);
        }
        catch (IllegalAccessException | IllegalArgumentException e)
        {
            ItemScroller.logger.warn("Exception while trying to reflect GuiContainer.hoveredSlot", e);
        }

        return slot;
    }

    public static int getGuiLeft(GuiContainer gui)
    {
        int value = 0;

        try
        {
            value = (int) field_GuiContainer_guiLeft.get(gui);
        }
        catch (IllegalAccessException | IllegalArgumentException e)
        {
            ItemScroller.logger.warn("Exception while trying to reflect GuiContainer.guiLeft", e);
        }

        return value;
    }

    public static int getGuiTop(GuiContainer gui)
    {
        int value = 0;

        try
        {
            value = (int) field_GuiContainer_guiTop.get(gui);
        }
        catch (IllegalAccessException | IllegalArgumentException e)
        {
            ItemScroller.logger.warn("Exception while trying to reflect GuiContainer.guiTop", e);
        }

        return value;
    }

    public static int getGuiXSize(GuiContainer gui)
    {
        int value = 0;

        try
        {
            value = (int) field_GuiContainer_xSize.get(gui);
        }
        catch (IllegalAccessException | IllegalArgumentException e)
        {
            ItemScroller.logger.warn("Exception while trying to reflect GuiContainer.xSize", e);
        }

        return value;
    }

    public static int getGuiYSize(GuiContainer gui)
    {
        int value = 0;

        try
        {
            value = (int) field_GuiContainer_ySize.get(gui);
        }
        catch (IllegalAccessException | IllegalArgumentException e)
        {
            ItemScroller.logger.warn("Exception while trying to reflect GuiContainer.ySize", e);
        }

        return value;
    }
}
