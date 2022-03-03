package fi.dy.masa.itemscroller.event;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.screen.ingame.CreativeInventoryScreen;
import net.minecraft.client.gui.screen.ingame.HandledScreen;
import net.minecraft.screen.slot.Slot;
import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.gui.GuiConfigs;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.RecipePattern;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.ClickPacketBuffer;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.util.MoveAction;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.Message;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.interfaces.IClientTickHandler;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.InfoUtils;

public class KeybindCallbacks implements IHotkeyCallback, IClientTickHandler
{
    private static final KeybindCallbacks INSTANCE = new KeybindCallbacks();

    protected int massCraftTicker;

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
        return Configs.Generic.MOD_MAIN_TOGGLE.getBooleanValue();
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        if (Configs.Generic.RATE_LIMIT_CLICK_PACKETS.getBooleanValue())
        {
            ClickPacketBuffer.setShouldBufferClickPackets(true);
        }

        boolean cancel = this.onKeyActionImpl(action, key);

        ClickPacketBuffer.setShouldBufferClickPackets(false);

        return cancel;
    }

    private boolean onKeyActionImpl(KeyAction action, IKeybind key)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc.player == null || mc.world == null)
        {
            return false;
        }

        if (key == Hotkeys.TOGGLE_MOD_ON_OFF.getKeybind())
        {
            Configs.Generic.MOD_MAIN_TOGGLE.toggleBooleanValue();
            String msg = this.functionalityEnabled() ? "itemscroller.message.toggled_mod_on" : "itemscroller.message.toggled_mod_off";
            InfoUtils.showGuiOrInGameMessage(Message.MessageType.INFO, msg);
            return true;
        }
        else if (key == Hotkeys.OPEN_CONFIG_GUI.getKeybind())
        {
            GuiBase.openGui(new GuiConfigs());
            return true;
        }

        if (this.functionalityEnabled() == false ||
            (GuiUtils.getCurrentScreen() instanceof HandledScreen) == false ||
            Configs.GUI_BLACKLIST.contains(GuiUtils.getCurrentScreen().getClass().getName()))
        {
            return false;
        }

        HandledScreen<?> gui = (HandledScreen<?>) GuiUtils.getCurrentScreen();
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);
        RecipeStorage recipes = RecipeStorage.getInstance();
        MoveAction moveAction = InputUtils.getDragMoveAction(key);

        if (slot != null)
        {
            if (moveAction != MoveAction.NONE)
            {
                final int mouseX = fi.dy.masa.malilib.util.InputUtils.getMouseX();
                final int mouseY = fi.dy.masa.malilib.util.InputUtils.getMouseY();
                return InventoryUtils.dragMoveItems(gui, moveAction, mouseX, mouseY, true);
            }
            else if (key == Hotkeys.KEY_MOVE_EVERYTHING.getKeybind())
            {
                InventoryUtils.tryMoveStacks(slot, gui, false, true, false);
                return true;
            }
            else if (key == Hotkeys.DROP_ALL_MATCHING.getKeybind())
            {
                if (Configs.Toggles.DROP_MATCHING.getBooleanValue() &&
                    Configs.GUI_BLACKLIST.contains(gui.getClass().getName()) == false &&
                    slot.hasStack())
                {
                    InventoryUtils.dropStacks(gui, slot.getStack(), slot, true);
                    return true;
                }
            }
        }

        if (key == Hotkeys.CRAFT_EVERYTHING.getKeybind())
        {
            InventoryUtils.craftEverythingPossibleWithCurrentRecipe(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.THROW_CRAFT_RESULTS.getKeybind())
        {
            InventoryUtils.throwAllCraftingResultsToGround(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.MOVE_CRAFT_RESULTS.getKeybind())
        {
            InventoryUtils.moveAllCraftingResultsToOtherInventory(recipes.getSelectedRecipe(), gui);
            return true;
        }
        else if (key == Hotkeys.STORE_RECIPE.getKeybind())
        {
            if (InputUtils.isRecipeViewOpen() && InventoryUtils.isCraftingSlot(gui, slot))
            {
                recipes.storeCraftingRecipeToCurrentSelection(slot, gui, true);
                return true;
            }
        }
        else if (key == Hotkeys.VILLAGER_TRADE_FAVORITES.getKeybind())
        {
            return InventoryUtils.villagerTradeEverythingPossibleWithAllFavoritedTrades();
        }
        else if (key == Hotkeys.SLOT_DEBUG.getKeybind())
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

    @Override
    public void onClientTick(MinecraftClient mc)
    {
        if (this.functionalityEnabled() == false || mc.player == null)
        {
            return;
        }

        ClickPacketBuffer.sendBufferedPackets(Configs.Generic.PACKET_RATE_LIMIT.getIntegerValue());

        if (ClickPacketBuffer.shouldCancelWindowClicks())
        {
            return;
        }

        if (GuiUtils.getCurrentScreen() instanceof HandledScreen<?> gui &&
            (GuiUtils.getCurrentScreen() instanceof CreativeInventoryScreen) == false &&
            Configs.GUI_BLACKLIST.contains(GuiUtils.getCurrentScreen().getClass().getName()) == false &&
            Hotkeys.MASS_CRAFT.getKeybind().isKeybindHeld())
        {
            if (++this.massCraftTicker < Configs.Generic.MASS_CRAFT_INTERVAL.getIntegerValue())
            {
                return;
            }

            Slot outputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);

            if (outputSlot != null)
            {
                if (Configs.Generic.RATE_LIMIT_CLICK_PACKETS.getBooleanValue())
                {
                    ClickPacketBuffer.setShouldBufferClickPackets(true);
                }

                RecipePattern recipe = RecipeStorage.getInstance().getSelectedRecipe();
                int limit = Configs.Generic.MASS_CRAFT_ITERATIONS.getIntegerValue();

                if (Configs.Generic.MASS_CRAFT_SWAPS.getBooleanValue())
                {
                    for (int i = 0; i < limit; ++i)
                    {
                        InventoryUtils.tryClearCursor(gui);
                        InventoryUtils.throwAllCraftingResultsToGround(recipe, gui);
                        InventoryUtils.setCraftingGridContentsUsingSwaps(gui, mc.player.getInventory(), recipe, outputSlot);

                        if (InventoryUtils.areStacksEqual(outputSlot.getStack(), recipe.getResult()) == false)
                        {
                            break;
                        }

                        InventoryUtils.shiftClickSlot(gui, outputSlot.id);
                    }
                }
                else
                {
                    int failsafe = 0;

                    while (++failsafe < limit)
                    {
                        InventoryUtils.tryClearCursor(gui);
                        InventoryUtils.setInhibitCraftingOutputUpdate(true);
                        InventoryUtils.throwAllCraftingResultsToGround(recipe, gui);
                        InventoryUtils.throwAllNonRecipeItemsToGround(recipe, gui);
                        InventoryUtils.tryMoveItemsToFirstCraftingGrid(recipe, gui, true);
                        InventoryUtils.setInhibitCraftingOutputUpdate(true);
                        InventoryUtils.updateCraftingOutputSlot(outputSlot);

                        if (InventoryUtils.areStacksEqual(outputSlot.getStack(), recipe.getResult()) == false)
                        {
                            break;
                        }

                        if (Configs.Generic.CARPET_CTRL_Q_CRAFTING.getBooleanValue())
                        {
                            InventoryUtils.dropStack(gui, outputSlot.id);
                        }
                        else
                        {
                            InventoryUtils.dropStacksWhileHasItem(gui, outputSlot.id, recipe.getResult());
                        }
                    }
                }

                ClickPacketBuffer.setShouldBufferClickPackets(false);
            }

            this.massCraftTicker = 0;
        }
    }
}
