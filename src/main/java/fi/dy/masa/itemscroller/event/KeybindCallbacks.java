package fi.dy.masa.itemscroller.event;

import fi.dy.masa.itemscroller.LiteModItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.CraftingRecipe;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
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
    private RecipeStorage recipes;

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

    public RecipeStorage getRecipes()
    {
        if (this.recipes == null)
        {
            this.recipes = new RecipeStorage(18, Configs.Generic.SCROLL_CRAFT_RECIPE_FILE_GLOBAL.getBooleanValue());
        }

        return this.recipes;
    }

    public void onWorldChanged()
    {
        if (Configs.Generic.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getBooleanValue())
        {
            this.getRecipes().readFromDisk();
        }
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        Minecraft mc = Minecraft.getMinecraft();

        System.out.printf("callbacks start\n");
        if (mc == null || mc.player == null || (mc.currentScreen instanceof GuiContainer) == false)
        {
            System.out.printf("callbacks abort\n");
            return false;
        }

        GuiContainer gui = (GuiContainer) mc.currentScreen;
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        RecipeStorage recipes = this.getRecipes();

        if (key == Hotkeys.KEY_MOVE_STACK_TO_OFFHAND.getKeybind())
        {
            // Swap the hovered stack to the Offhand
            if (Configs.Toggles.OFFHAND_SWAP.getBooleanValue() &&
                (gui instanceof GuiInventory) && slot != null)
            {
                System.out.printf("callbacks KEY_MOVE_STACK_TO_OFFHAND\n");
                InventoryUtils.swapSlots(gui, slot.slotNumber, 45);
                return true;
            }
        }
        else if (key == Hotkeys.KEY_CRAFT_EVERYTHING.getKeybind())
        {
            System.out.printf("callbacks KEY_CRAFT_EVERYTHING\n");
            InventoryUtils.craftEverythingPossibleWithCurrentRecipe(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.KEY_THROW_CRAFT_RESULTS.getKeybind())
        {
            System.out.printf("callbacks KEY_THROW_CRAFT_RESULTS\n");
            InventoryUtils.throwAllCraftingResultsToGround(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.KEY_MOVE_CRAFT_RESULTS.getKeybind())
        {
            InventoryUtils.moveAllCraftingResultsToOtherInventory(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.KEY_DROP_ALL_MATCHING.getKeybind())
        {
            System.out.printf("callbacks KEY_DROP_ALL_MATCHING 1, slot: %s\n", slot);
            if (Configs.Toggles.DROP_MATCHING.getBooleanValue() &&
                Configs.GUI_BLACKLIST.contains(gui.getClass().getName()) == false &&
                slot != null && slot.getHasStack())
            {
                System.out.printf("callbacks KEY_DROP_ALL_MATCHING 2\n");
                InventoryUtils.dropStacks(gui, slot.getStack(), slot, true);
                return true;
            }
        }
        else if (key == Hotkeys.KEY_MAIN_TOGGLE.getKeybind())
        {
            System.out.printf("callbacks KEY_MAIN_TOGGLE\n");
            this.disabled = ! this.disabled;

            if (this.disabled)
            {
                mc.player.playSound(SoundEvents.BLOCK_NOTE_BASS, 0.8f, 0.8f);
            }
            else
            {
                mc.player.playSound(SoundEvents.BLOCK_NOTE_PLING, 0.5f, 1.0f);
            }

            return true;
        }
        else if (key == Hotkeys.KEY_SLOT_DEBUG.getKeybind())
        {
            System.out.printf("callbacks KEY_SLOT_DEBUG\n");
            if (slot != null)
            {
                InventoryUtils.debugPrintSlotInfo(gui, slot);
            }
            else
            {
                LiteModItemScroller.logger.info("GUI class: {}", gui.getClass().getName());
            }

            return true;
        }
        else if ((key == Hotkeys.KEY_DRAG_DROP_SINGLE.getKeybind() && Configs.Toggles.DRAG_DROP_SINGLE.getBooleanValue()) ||
                 (key == Hotkeys.KEY_DRAG_DROP_STACKS.getKeybind() && Configs.Toggles.DRAG_DROP_STACKS.getBooleanValue()) ||
                 (key == Hotkeys.KEY_DRAG_FULL_STACKS.getKeybind() && Configs.Toggles.DRAG_MOVE_STACKS.getBooleanValue()) ||
                 (key == Hotkeys.KEY_DRAG_LEAVE_ONE.getKeybind() && Configs.Toggles.DRAG_MOVE_LEAVE_ONE.getBooleanValue()) ||
                 (key == Hotkeys.KEY_DRAG_MOVE_ONE.getKeybind() && Configs.Toggles.DRAG_MOVE_ONE.getBooleanValue()))
        {
            System.out.printf("callbacks dragMoveItems\n");
            return InventoryUtils.dragMoveItems(gui, mc, true);
        }

        System.out.printf("callbacks end\n");
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
                CraftingRecipe recipe = this.getRecipes().getSelectedRecipe();

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
