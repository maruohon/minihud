package fi.dy.masa.itemscroller.event;

import org.lwjgl.glfw.GLFW;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.gui.widgets.WidgetTradeList;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.IGuiMerchant;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.util.MoveAction;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybindManager;
import fi.dy.masa.malilib.hotkeys.IKeybindProvider;
import fi.dy.masa.malilib.hotkeys.IKeyboardInputHandler;
import fi.dy.masa.malilib.hotkeys.IMouseInputHandler;
import fi.dy.masa.malilib.util.KeyCodes;
import net.minecraft.client.MainWindow;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.entity.passive.EntityVillager;
import net.minecraft.inventory.ContainerMerchant;
import net.minecraft.inventory.Slot;
import net.minecraft.network.play.client.CPacketSelectTrade;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.village.MerchantRecipeList;

public class InputHandler implements IKeybindProvider, IKeyboardInputHandler, IMouseInputHandler
{
    private final KeybindCallbacks callbacks;

    public InputHandler()
    {
        this.callbacks = KeybindCallbacks.getInstance();
    }

    @Override
    public void addKeysToMap(IKeybindManager manager)
    {
        for (IHotkey hotkey : Hotkeys.HOTKEY_LIST)
        {
            manager.addKeybindToMap(hotkey.getKeybind());
        }
    }

    @Override
    public void addHotkeys(IKeybindManager manager)
    {
        manager.addHotkeysForCategory(Reference.MOD_NAME, "itemscroller.hotkeys.category.hotkeys", Hotkeys.HOTKEY_LIST);
    }

    @Override
    public boolean onKeyInput(int keyCode, int scanCode, int modifiers, boolean eventKeyState)
    {
        if (InputUtils.isRecipeViewOpen() && eventKeyState)
        {
            int index = -1;
            RecipeStorage recipes = RecipeStorage.getInstance();
            int oldIndex = recipes.getSelection();
            int recipesPerPage = recipes.getRecipeCountPerPage();
            int recipeIndexChange = GuiScreen.isShiftKeyDown() ? recipesPerPage : recipesPerPage / 2;

            if (keyCode >= KeyCodes.KEY_1 && keyCode <= KeyCodes.KEY_9)
            {
                index = MathHelper.clamp(keyCode - GLFW.GLFW_KEY_1, 0, 8);
            }
            else if (keyCode == KeyCodes.KEY_UP && oldIndex > 0)
            {
                index = oldIndex - 1;
            }
            else if (keyCode == KeyCodes.KEY_DOWN && oldIndex < (recipes.getTotalRecipeCount() - 1))
            {
                index = oldIndex + 1;
            }
            else if (keyCode == KeyCodes.KEY_LEFT && oldIndex >= recipeIndexChange)
            {
                index = oldIndex - recipeIndexChange;
            }
            else if (keyCode == KeyCodes.KEY_RIGHT && oldIndex < (recipes.getTotalRecipeCount() - recipeIndexChange))
            {
                index = oldIndex + recipeIndexChange;
            }

            if (index >= 0)
            {
                recipes.changeSelectedRecipe(index);
                return true;
            }
        }

        return this.handleInput(keyCode, eventKeyState, 0);
    }

    @Override
    public boolean onMouseScroll(int mouseX, int mouseY, double amount)
    {
        return this.handleInput(KeyCodes.KEY_NONE, false, amount);
    }

    @Override
    public boolean onMouseClick(int mouseX, int mouseY, int eventButton, boolean eventButtonState)
    {
        return this.handleInput(eventButton - 100, eventButtonState, 0);
    }

    private boolean handleInput(int keyCode, boolean keyState, double dWheel)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (action != MoveAction.NONE && InputUtils.isActionKeyActive(action) == false)
        {
            InventoryUtils.stopDragging();
        }

        Minecraft mc = Minecraft.getInstance();
        boolean cancel = false;

        if (this.callbacks.functionalityEnabled() && mc.player != null)
        {
            final boolean isAttack = InputUtils.isAttack(keyCode);
            final boolean isUse = InputUtils.isUse(keyCode);
            final boolean isPickBlock = InputUtils.isPickBlock(keyCode);
            final boolean isAttackUseOrPick = isAttack || isUse || isPickBlock;
            MainWindow window = mc.mainWindow;
            final int mouseX = (int) (mc.mouseHelper.getMouseX() * (double) window.getScaledWidth() / (double) window.getWidth());
            final int mouseY = (int) (mc.mouseHelper.getMouseY() * (double) window.getScaledHeight() / (double) window.getHeight());

            if (Configs.Toggles.VILLAGER_TRADE_LIST.getBooleanValue())
            {
                VillagerDataStorage storage = VillagerDataStorage.getInstance();

                if (mc.currentScreen == null && mc.objectMouseOver != null &&
                    mc.objectMouseOver.type == RayTraceResult.Type.ENTITY &&
                    mc.objectMouseOver.entity instanceof EntityVillager)
                {
                    storage.setLastInteractedUUID(mc.objectMouseOver.entity.getUniqueID());
                }
                else if (mc.currentScreen instanceof GuiMerchant && storage.hasInteractionTarget())
                {
                    WidgetTradeList widget = ((IGuiMerchant) mc.currentScreen).getTradeListWidget();

                    if (widget != null)
                    {
                        int mouseButton = isAttack ? 0 : (isUse ? 1 : 2);

                        if (widget.isMouseOver(mouseX, mouseY))
                        {
                            if (dWheel != 0)
                            {
                                widget.onMouseScrolled(mouseX, mouseY, dWheel);
                                return true;
                            }
                            else if (keyState && isAttackUseOrPick)
                            {
                                widget.onMouseClicked(mouseX, mouseY, mouseButton);
                                return true;
                            }
                        }

                        if (keyState == false && dWheel == 0)
                        {
                            widget.onMouseReleased(mouseX, mouseY, mouseButton);
                        }
                    }
                }
            }

            if (mc.currentScreen instanceof GuiContainer &&
                (mc.currentScreen instanceof GuiContainerCreative) == false &&
                Configs.GUI_BLACKLIST.contains(mc.currentScreen.getClass().getName()) == false)
            {
                GuiContainer gui = (GuiContainer) mc.currentScreen;
                RecipeStorage recipes = RecipeStorage.getInstance();

                if (dWheel != 0)
                {
                    // When scrolling while the recipe view is open, change the selection instead of moving items
                    if (InputUtils.isRecipeViewOpen())
                    {
                        recipes.scrollSelection(dWheel < 0);
                        cancel = true;
                    }
                    else
                    {
                        cancel = InventoryUtils.tryMoveItems(gui, recipes, dWheel > 0);
                    }
                }
                else
                {
                    Slot slot = AccessorUtils.getSlotUnderMouse(gui);
                    final boolean isShiftDown = GuiScreen.isShiftKeyDown();

                    if (keyState && isAttackUseOrPick)
                    {
                        int hoveredRecipeId = RenderEventHandler.instance().getHoveredRecipeId(mouseX, mouseY, recipes, gui);

                        // Hovering over an item in the recipe view
                        if (hoveredRecipeId >= 0)
                        {
                            InventoryUtils.handleRecipeClick(gui, mc, recipes, hoveredRecipeId, isAttack, isUse, isPickBlock, isShiftDown);
                            return true;
                        }
                        // Pick-blocking over a crafting output slot with the recipe view open, store the recipe
                        else if (isPickBlock && InputUtils.isRecipeViewOpen() && InventoryUtils.isCraftingSlot(gui, slot))
                        {
                            recipes.storeCraftingRecipeToCurrentSelection(slot, gui, true);
                            cancel = true;
                        }
                    }

                    InventoryUtils.checkForItemPickup(gui, mc);

                    if (keyState && (isAttack || isUse))
                    {
                        InventoryUtils.storeSourceSlotCandidate(slot, mc);
                    }

                    if (Configs.Toggles.RIGHT_CLICK_CRAFT_STACK.getBooleanValue() &&
                        isUse && keyState &&
                        InventoryUtils.isCraftingSlot(gui, slot))
                    {
                        InventoryUtils.rightClickCraftOneStack(gui);
                    }
                    else if (Configs.Toggles.SHIFT_PLACE_ITEMS.getBooleanValue() &&
                             isAttack && isShiftDown &&
                             InventoryUtils.canShiftPlaceItems(gui))
                    {
                        cancel |= InventoryUtils.shiftPlaceItems(slot, gui);
                    }
                    else if (Configs.Toggles.SHIFT_DROP_ITEMS.getBooleanValue() &&
                             isAttack && isShiftDown &&
                             InputUtils.canShiftDropItems(gui, mc, mouseX, mouseY))
                    {
                        cancel |= InventoryUtils.shiftDropItems(gui);
                    }
                }
            }
        }

        return cancel;
    }

    @Override
    public void onMouseMove(int mouseX, int mouseY)
    {
        Minecraft mc = Minecraft.getInstance();

        if (this.callbacks.functionalityEnabled() &&
            mc.player != null &&
            mc.currentScreen instanceof GuiContainer &&
            Configs.GUI_BLACKLIST.contains(mc.currentScreen.getClass().getName()) == false)
        {
            this.handleDragging((GuiContainer) mc.currentScreen, mc, mouseX, mouseY, false);
        }
    }

    private boolean handleDragging(GuiContainer gui, Minecraft mc, int mouseX, int mouseY, boolean isClick)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (InputUtils.isActionKeyActive(action))
        {
            return InventoryUtils.dragMoveItems(gui, mc, action, mouseX, mouseY, false);
        }
        else if (action != MoveAction.NONE)
        {
            InventoryUtils.stopDragging();
        }

        return false;
    }

    public static void changeTradePage(GuiMerchant gui, int page)
    {
        Minecraft mc = Minecraft.getInstance();
        MerchantRecipeList trades = gui.getMerchant().getRecipes(mc.player);

        // The trade list is unfortunately synced after the GUI
        // opens, so the trade list can be null here when we want to
        // restore the last viewed page when the GUI first opens
        if (page >= 0 && (trades == null || page < trades.size()))
        {
            ((IGuiMerchant) gui).setSelectedMerchantRecipe(page);
        }

        ((ContainerMerchant) gui.inventorySlots).setCurrentRecipeIndex(page);
        mc.getConnection().sendPacket(new CPacketSelectTrade(page));
    }
}
