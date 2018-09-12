package fi.dy.masa.itemscroller.util;

import org.lwjgl.input.Keyboard;
import org.lwjgl.input.Mouse;
import fi.dy.masa.itemscroller.config.Configs.Toggles;
import fi.dy.masa.itemscroller.config.Hotkeys;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.ScaledResolution;
import net.minecraft.client.gui.inventory.GuiContainer;

public class InputUtils
{
    public static int getMouseX()
    {
        Minecraft mc = Minecraft.getMinecraft();
        ScaledResolution scaledresolution = new ScaledResolution(mc);
        int w = scaledresolution.getScaledWidth();
        return Mouse.getX() * w / mc.displayWidth;
    }

    public static int getMouseY()
    {
        Minecraft mc = Minecraft.getMinecraft();
        ScaledResolution scaledresolution = new ScaledResolution(mc);
        int h = scaledresolution.getScaledHeight();
        return h - Mouse.getY() * h / mc.displayHeight - 1;
    }

    public static boolean isRecipeViewOpen()
    {
        return Hotkeys.KEY_RECIPE_VIEW.getKeybind().isKeybindHeld();
    }

    public static boolean canShiftDropItems(GuiContainer gui, Minecraft mc)
    {
        if (InventoryUtils.isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            int left = AccessorUtils.getGuiLeft(gui);
            int top = AccessorUtils.getGuiTop(gui);
            int xSize = AccessorUtils.getGuiXSize(gui);
            int ySize = AccessorUtils.getGuiYSize(gui);
            int mouseAbsX = Mouse.getEventX() * gui.width / mc.displayWidth;
            int mouseAbsY = gui.height - Mouse.getEventY() * gui.height / mc.displayHeight - 1;
            boolean isOutsideGui = mouseAbsX < left || mouseAbsY < top || mouseAbsX >= left + xSize || mouseAbsY >= top + ySize;

            return isOutsideGui && AccessorUtils.getSlotAtPosition(gui, mouseAbsX - left, mouseAbsY - top) == null;
        }

        return false;
    }

    public static MoveType getDragMoveType(Minecraft mc)
    {
        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);
        boolean isShiftDown = GuiScreen.isShiftKeyDown();
        boolean isControlDown = GuiScreen.isCtrlKeyDown();
        boolean eitherMouseButtonDown = leftButtonDown || rightButtonDown;

        // Only one of the mouse buttons is down, and only one of shift or control is down
        if (leftButtonDown ^ rightButtonDown)
        {
            if (shouldMoveVertically())
            {
                if (Keyboard.isKeyDown(Keyboard.KEY_W))
                {
                    return MoveType.MOVE_UP;
                }
                else if (Keyboard.isKeyDown(Keyboard.KEY_S))
                {
                    return MoveType.MOVE_DOWN;
                }
            }
            else if (isShiftDown ^ isControlDown)
            {
                boolean dropKeyDown = isKeybindHeld(mc.gameSettings.keyBindDrop.getKeyCode());

                if (dropKeyDown &&
                    ((isShiftDown && Toggles.DRAG_DROP_STACKS.getBooleanValue()) ||
                     (isControlDown && Toggles.DRAG_DROP_SINGLE.getBooleanValue())))
                {
                    return MoveType.DROP;
                }
                else if ((isShiftDown && leftButtonDown && Toggles.DRAG_MOVE_STACKS.getBooleanValue()) ||
                    (isShiftDown && rightButtonDown && Toggles.DRAG_MOVE_LEAVE_ONE.getBooleanValue()) ||
                    (isControlDown && eitherMouseButtonDown && Toggles.DRAG_MOVE_ONE.getBooleanValue()))
                {
                    return MoveType.MOVE_TO_OTHER;
                }
            }
        }

        return MoveType.INVALID;
    }

    public static MoveAmount getDragMoveAmount(MoveType type, Minecraft mc)
    {
        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);
        boolean isShiftDown = GuiScreen.isShiftKeyDown();
        boolean isControlDown = GuiScreen.isCtrlKeyDown();

        // Only one of the mouse buttons is down, and only one of shift or control is down
        if (leftButtonDown ^ rightButtonDown)
        {
            if (isShiftDown ^ isControlDown)
            {
                if (isControlDown && isShiftDown == false)
                {
                    return MoveAmount.MOVE_ONE;
                }
                else if (rightButtonDown && isShiftDown)
                {
                    return MoveAmount.LEAVE_ONE;
                }
                else if (leftButtonDown && isShiftDown)
                {
                    return MoveAmount.MOVE_ALL;
                }
            }
            // Allow moving entire stacks with just W or S down, (without Shift),
            // but only when first clicking the left button down, and when not holding Control
            else if (leftButtonDown &&
                     isShiftDown == false &&
                     isControlDown == false &&
                     (type == MoveType.MOVE_UP || type == MoveType.MOVE_DOWN) &&
                     Mouse.getEventButtonState())
            {
                return MoveAmount.MOVE_ALL;
            }
        }

        return MoveAmount.INVALID;
    }

    public static boolean shouldMoveVertically()
    {
        return Toggles.WS_CLICKING.getBooleanValue() && (Keyboard.isKeyDown(Keyboard.KEY_W) || Keyboard.isKeyDown(Keyboard.KEY_S));
    }

    public static boolean isKeybindHeld(int keyCode)
    {
        if (keyCode > 0 && keyCode < Keyboard.getKeyCount())
        {
            return Keyboard.isKeyDown(keyCode);
        }
        else
        {
            keyCode += 100;
            return keyCode >= 0 && keyCode < Mouse.getButtonCount() && Mouse.isButtonDown(keyCode);
        }
    }

    public static boolean mouseEventIsLeftClick()
    {
        return Mouse.getEventButton() == Minecraft.getMinecraft().gameSettings.keyBindAttack.getKeyCode() + 100;
    }

    public static boolean mouseEventIsRightClick()
    {
        return Mouse.getEventButton() == Minecraft.getMinecraft().gameSettings.keyBindUseItem.getKeyCode() + 100;
    }

    public static boolean mouseEventIsPickBlock()
    {
        return Mouse.getEventButton() == Minecraft.getMinecraft().gameSettings.keyBindPickBlock.getKeyCode() + 100;
    }
}
