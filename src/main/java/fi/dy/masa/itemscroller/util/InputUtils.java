package fi.dy.masa.itemscroller.util;

import org.lwjgl.input.Mouse;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.event.KeybindCallbacks;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.util.GuiUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.inventory.GuiContainer;

public class InputUtils
{
    public static int getMouseX()
    {
        int width = GuiUtils.getScaledWindowWidth();
        return Mouse.getX() * width / GuiUtils.getDisplayWidth();
    }

    public static int getMouseY()
    {
        int height = GuiUtils.getScaledWindowHeight();
        return height - Mouse.getY() * height / GuiUtils.getDisplayHeight() - 1;
    }

    public static boolean isRecipeViewOpen()
    {
        return GuiUtils.getCurrentScreen() != null &&
               Hotkeys.KEY_RECIPE_VIEW.getKeybind().isKeybindHeld() &&
               KeybindCallbacks.getInstance().functionalityEnabled() &&
               CraftingHandler.isCraftingGui(GuiUtils.getCurrentScreen());
    }

    public static boolean canShiftDropItems(GuiContainer gui, Minecraft mc)
    {
        if (InventoryUtils.isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            int left = AccessorUtils.getGuiLeft(gui);
            int top = AccessorUtils.getGuiTop(gui);
            int xSize = AccessorUtils.getGuiXSize(gui);
            int ySize = AccessorUtils.getGuiYSize(gui);
            int mouseAbsX = Mouse.getEventX() * gui.width / GuiUtils.getDisplayWidth();
            int mouseAbsY = gui.height - Mouse.getEventY() * gui.height / GuiUtils.getDisplayHeight() - 1;
            boolean isOutsideGui = mouseAbsX < left || mouseAbsY < top || mouseAbsX >= left + xSize || mouseAbsY >= top + ySize;

            return isOutsideGui && AccessorUtils.getSlotAtPosition(gui, mouseAbsX - left, mouseAbsY - top) == null;
        }

        return false;
    }

    public static MoveAction getDragMoveAction(IKeybind key)
    {
             if (key == Hotkeys.KEY_DRAG_FULL_STACKS.getKeybind())      { return MoveAction.MOVE_TO_OTHER_STACKS;       }
        else if (key == Hotkeys.KEY_DRAG_LEAVE_ONE.getKeybind())        { return MoveAction.MOVE_TO_OTHER_LEAVE_ONE;    }
        else if (key == Hotkeys.KEY_DRAG_MOVE_ONE.getKeybind())         { return MoveAction.MOVE_TO_OTHER_MOVE_ONE;     }
        else if (key == Hotkeys.KEY_DRAG_MATCHING.getKeybind())         { return MoveAction.MOVE_TO_OTHER_MATCHING;     }

        else if (key == Hotkeys.KEY_DRAG_DROP_STACKS.getKeybind())      { return MoveAction.DROP_STACKS;                }
        else if (key == Hotkeys.KEY_DRAG_DROP_LEAVE_ONE.getKeybind())   { return MoveAction.DROP_LEAVE_ONE;             }
        else if (key == Hotkeys.KEY_DRAG_DROP_SINGLE.getKeybind())      { return MoveAction.DROP_ONE;                   }

        else if (key == Hotkeys.KEY_WS_MOVE_UP_STACKS.getKeybind())     { return MoveAction.MOVE_UP_STACKS;             }
        else if (key == Hotkeys.KEY_WS_MOVE_UP_MATCHING.getKeybind())   { return MoveAction.MOVE_UP_MATCHING;           }
        else if (key == Hotkeys.KEY_WS_MOVE_UP_LEAVE_ONE.getKeybind())  { return MoveAction.MOVE_UP_LEAVE_ONE;          }
        else if (key == Hotkeys.KEY_WS_MOVE_UP_SINGLE.getKeybind())     { return MoveAction.MOVE_UP_MOVE_ONE;           }
        else if (key == Hotkeys.KEY_WS_MOVE_DOWN_STACKS.getKeybind())   { return MoveAction.MOVE_DOWN_STACKS;           }
        else if (key == Hotkeys.KEY_WS_MOVE_DOWN_MATCHING.getKeybind()) { return MoveAction.MOVE_DOWN_MATCHING;         }
        else if (key == Hotkeys.KEY_WS_MOVE_DOWN_LEAVE_ONE.getKeybind()){ return MoveAction.MOVE_DOWN_LEAVE_ONE;        }
        else if (key == Hotkeys.KEY_WS_MOVE_DOWN_SINGLE.getKeybind())   { return MoveAction.MOVE_DOWN_MOVE_ONE;         }

        return MoveAction.NONE;
    }

    public static boolean isActionKeyActive(MoveAction action)
    {
        switch (action)
        {
            case MOVE_TO_OTHER_STACKS:          return Hotkeys.KEY_DRAG_FULL_STACKS.getKeybind().isKeybindHeld();
            case MOVE_TO_OTHER_LEAVE_ONE:       return Hotkeys.KEY_DRAG_LEAVE_ONE.getKeybind().isKeybindHeld();
            case MOVE_TO_OTHER_MOVE_ONE:        return Hotkeys.KEY_DRAG_MOVE_ONE.getKeybind().isKeybindHeld();
            case MOVE_TO_OTHER_MATCHING:        return Hotkeys.KEY_DRAG_MATCHING.getKeybind().isKeybindHeld();
            case MOVE_TO_OTHER_EVERYTHING:      return Hotkeys.KEY_MOVE_EVERYTHING.getKeybind().isKeybindHeld();
            case DROP_STACKS:                   return Hotkeys.KEY_DRAG_DROP_STACKS.getKeybind().isKeybindHeld();
            case DROP_LEAVE_ONE:                return Hotkeys.KEY_DRAG_DROP_LEAVE_ONE.getKeybind().isKeybindHeld();
            case DROP_ONE:                      return Hotkeys.KEY_DRAG_DROP_SINGLE.getKeybind().isKeybindHeld();
            case MOVE_UP_STACKS:                return Hotkeys.KEY_WS_MOVE_UP_STACKS.getKeybind().isKeybindHeld();
            case MOVE_UP_MATCHING:              return Hotkeys.KEY_WS_MOVE_UP_MATCHING.getKeybind().isKeybindHeld();
            case MOVE_UP_LEAVE_ONE:             return Hotkeys.KEY_WS_MOVE_UP_LEAVE_ONE.getKeybind().isKeybindHeld();
            case MOVE_UP_MOVE_ONE:              return Hotkeys.KEY_WS_MOVE_UP_SINGLE.getKeybind().isKeybindHeld();
            case MOVE_DOWN_STACKS:              return Hotkeys.KEY_WS_MOVE_DOWN_STACKS.getKeybind().isKeybindHeld();
            case MOVE_DOWN_MATCHING:            return Hotkeys.KEY_WS_MOVE_DOWN_MATCHING.getKeybind().isKeybindHeld();
            case MOVE_DOWN_LEAVE_ONE:           return Hotkeys.KEY_WS_MOVE_DOWN_LEAVE_ONE.getKeybind().isKeybindHeld();
            case MOVE_DOWN_MOVE_ONE:            return Hotkeys.KEY_WS_MOVE_DOWN_SINGLE.getKeybind().isKeybindHeld();
            default:
        }

        return false;
    }

    public static MoveAmount getMoveAmount(MoveAction action)
    {
        switch (action)
        {
            case SCROLL_TO_OTHER_MOVE_ONE:
            case MOVE_TO_OTHER_MOVE_ONE:
            case DROP_ONE:
            case MOVE_DOWN_MOVE_ONE:
            case MOVE_UP_MOVE_ONE:
                return MoveAmount.MOVE_ONE;

            case MOVE_TO_OTHER_LEAVE_ONE:
            case DROP_LEAVE_ONE:
            case MOVE_DOWN_LEAVE_ONE:
            case MOVE_UP_LEAVE_ONE:
                return MoveAmount.LEAVE_ONE;

            case SCROLL_TO_OTHER_STACKS:
            case MOVE_TO_OTHER_STACKS:
            case DROP_STACKS:
            case MOVE_DOWN_STACKS:
            case MOVE_UP_STACKS:
                return MoveAmount.FULL_STACKS;

            case SCROLL_TO_OTHER_MATCHING:
            case MOVE_TO_OTHER_MATCHING:
            case DROP_ALL_MATCHING:
            case MOVE_UP_MATCHING:
            case MOVE_DOWN_MATCHING:
                return MoveAmount.ALL_MATCHING;

            case MOVE_TO_OTHER_EVERYTHING:
            case SCROLL_TO_OTHER_EVERYTHING:
                return MoveAmount.EVERYTHING;

            default:
        }

        return MoveAmount.NONE;
    }

    public static boolean isAttack(int keyCode)
    {
        return keyCode == Minecraft.getMinecraft().gameSettings.keyBindAttack.getKeyCode();
    }

    public static boolean isUse(int keyCode)
    {
        return keyCode == Minecraft.getMinecraft().gameSettings.keyBindUseItem.getKeyCode();
    }

    public static boolean isPickBlock(int keyCode)
    {
        return keyCode == Minecraft.getMinecraft().gameSettings.keyBindPickBlock.getKeyCode();
    }
}
