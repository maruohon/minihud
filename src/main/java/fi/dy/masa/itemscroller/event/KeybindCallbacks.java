package fi.dy.masa.itemscroller.event;

import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.gui.GuiConfigs;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.CraftingRecipe;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.util.MoveAction;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import net.minecraft.client.MainWindow;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.init.SoundEvents;
import net.minecraft.inventory.Slot;

public class KeybindCallbacks implements IHotkeyCallback
{
    private static final KeybindCallbacks INSTANCE = new KeybindCallbacks();

    private boolean disabled;

    public static KeybindCallbacks getInstance()
    {
        return INSTANCE;
    }

    private KeybindCallbacks()
    {
    }

    public void setCallbacks()
    {
        for (ConfigHotkey hotkey : Hotkeys.HOTKEY_LIST)
        {
            hotkey.getKeybind().setCallback(this);
        }
    }

    public boolean functionalityEnabled()
    {
        return this.disabled == false;
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        Minecraft mc = Minecraft.getInstance();

        if (key == Hotkeys.KEY_MAIN_TOGGLE.getKeybind())
        {
            this.disabled = ! this.disabled;

            if (this.disabled)
            {
                mc.player.playSound(SoundEvents.BLOCK_NOTE_BLOCK_BASS, 0.8f, 0.8f);
            }
            else
            {
                mc.player.playSound(SoundEvents.BLOCK_NOTE_BLOCK_PLING, 0.5f, 1.0f);
            }

            return true;
        }
        else if (key == Hotkeys.KEY_OPEN_CONFIG_GUI.getKeybind())
        {
            mc.displayGuiScreen(new GuiConfigs());
            return true;
        }

        if (this.disabled || mc == null || mc.player == null || (mc.currentScreen instanceof GuiContainer) == false)
        {
            return false;
        }

        GuiContainer gui = (GuiContainer) mc.currentScreen;
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        RecipeStorage recipes = RecipeStorage.getInstance();
        MoveAction moveAction = InputUtils.getDragMoveAction(key);

        if (slot != null)
        {
            if (moveAction != MoveAction.NONE)
            {
                MainWindow window = mc.mainWindow;
                final int mouseX = (int) (mc.mouseHelper.getMouseX() * (double) window.getScaledWidth() / (double) window.getWidth());
                final int mouseY = (int) (mc.mouseHelper.getMouseY() * (double) window.getScaledHeight() / (double) window.getHeight());
                return InventoryUtils.dragMoveItems(gui, mc, moveAction, mouseX, mouseY, true);
            }
            else if (key == Hotkeys.KEY_MOVE_EVERYTHING.getKeybind())
            {
                InventoryUtils.tryMoveStacks(slot, gui, false, true, false);
                return true;
            }
            else if (key == Hotkeys.KEY_DROP_ALL_MATCHING.getKeybind())
            {
                if (Configs.Toggles.DROP_MATCHING.getBooleanValue() &&
                    Configs.GUI_BLACKLIST.contains(gui.getClass().getName()) == false &&
                    slot.getHasStack())
                {
                    InventoryUtils.dropStacks(gui, slot.getStack(), slot, true);
                    return true;
                }
            }
            else if (key == Hotkeys.KEY_MOVE_STACK_TO_OFFHAND.getKeybind())
            {
                // Swap the hovered stack to the Offhand
                if ((gui instanceof GuiInventory) && slot != null)
                {
                    InventoryUtils.swapSlots(gui, slot.slotNumber, 45);
                    return true;
                }
            }
        }

        if (key == Hotkeys.KEY_CRAFT_EVERYTHING.getKeybind())
        {
            InventoryUtils.craftEverythingPossibleWithCurrentRecipe(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.KEY_THROW_CRAFT_RESULTS.getKeybind())
        {
            InventoryUtils.throwAllCraftingResultsToGround(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.KEY_MOVE_CRAFT_RESULTS.getKeybind())
        {
            InventoryUtils.moveAllCraftingResultsToOtherInventory(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.KEY_STORE_RECIPE.getKeybind())
        {
            if (InputUtils.isRecipeViewOpen() && InventoryUtils.isCraftingSlot(gui, slot))
            {
                recipes.storeCraftingRecipeToCurrentSelection(slot, gui, true);
                return true;
            }
        }
        else if (key == Hotkeys.KEY_VILLAGER_TRADE_FAVORITES.getKeybind())
        {
            InventoryUtils.villagerTradeEverythingPossibleWithAllFavoritedTrades();
            return true;
        }
        else if (key == Hotkeys.KEY_SLOT_DEBUG.getKeybind())
        {
            if (slot != null)
            {
                InventoryUtils.debugPrintSlotInfo(gui, slot);
            }
            else
            {
                ItemScroller.logger.info("GUI class: {}", gui.getClass().getName());
            }

            return true;
        }

        return false;
    }

    public void onTick(Minecraft mc)
    {
        if (this.disabled == false &&
            mc != null &&
            mc.player != null &&
            mc.currentScreen instanceof GuiContainer &&
            (mc.currentScreen instanceof GuiContainerCreative) == false &&
            Configs.GUI_BLACKLIST.contains(mc.currentScreen.getClass().getName()) == false &&
            Hotkeys.KEY_MASS_CRAFT.getKeybind().isKeybindHeld())
        {
            GuiScreen guiScreen = mc.currentScreen;
            GuiContainer gui = (GuiContainer) guiScreen;
            Slot outputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

            if (outputSlot != null)
            {
                CraftingRecipe recipe = RecipeStorage.getInstance().getSelectedRecipe();

                InventoryUtils.tryClearCursor(gui, mc);
                InventoryUtils.throwAllCraftingResultsToGround(recipe, gui);
                InventoryUtils.tryMoveItemsToFirstCraftingGrid(recipe, gui, true);

                int failsafe = 0;

                while (++failsafe < 40 && InventoryUtils.areStacksEqual(outputSlot.getStack(), recipe.getResult()))
                {
                    if (Configs.Generic.CARPET_CTRL_Q_CRAFTING.getBooleanValue())
                    {
                        InventoryUtils.dropStack(gui, outputSlot.slotNumber);
                    }
                    else
                    {
                        InventoryUtils.dropStacksWhileHasItem(gui, outputSlot.slotNumber, recipe.getResult());
                    }

                    InventoryUtils.tryClearCursor(gui, mc);
                    InventoryUtils.throwAllCraftingResultsToGround(recipe, gui);
                    InventoryUtils.tryMoveItemsToFirstCraftingGrid(recipe, gui, true);
                }
            }
        }
    }
}
