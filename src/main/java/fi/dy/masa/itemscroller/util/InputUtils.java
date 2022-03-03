package fi.dy.masa.itemscroller.util;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.screen.ingame.HandledScreen;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.event.KeybindCallbacks;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeybindMulti;
import fi.dy.masa.malilib.util.GuiUtils;

public class InputUtils
{
    public static boolean isRecipeViewOpen()
    {
        return GuiUtils.getCurrentScreen() != null &&
               Hotkeys.RECIPE_VIEW.getKeybind().isKeybindHeld() &&
               KeybindCallbacks.getInstance().functionalityEnabled() &&
               CraftingHandler.isCraftingGui(GuiUtils.getCurrentScreen());
    }

    public static boolean canShiftDropItems(HandledScreen<?> gui, MinecraftClient mc, int mouseX, int mouseY)
    {
        if (InventoryUtils.isStackEmpty(gui.getScreenHandler().getCursorStack()) == false)
        {
            int left = AccessorUtils.getGuiLeft(gui);
            int top = AccessorUtils.getGuiTop(gui);
            int xSize = AccessorUtils.getGuiXSize(gui);
            int ySize = AccessorUtils.getGuiYSize(gui);
            boolean isOutsideGui = mouseX < left || mouseY < top || mouseX >= left + xSize || mouseY >= top + ySize;

            return isOutsideGui && AccessorUtils.getSlotAtPosition(gui, mouseX - left, mouseY - top) == null;
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
        return keyCode == KeybindMulti.getKeyCode(MinecraftClient.getInstance().options.attackKey);
    }

    public static boolean isUse(int keyCode)
    {
        return keyCode == KeybindMulti.getKeyCode(MinecraftClient.getInstance().options.useKey);
    }

    public static boolean isPickBlock(int keyCode)
    {
        return keyCode == KeybindMulti.getKeyCode(MinecraftClient.getInstance().options.pickItemKey);
    }
}
