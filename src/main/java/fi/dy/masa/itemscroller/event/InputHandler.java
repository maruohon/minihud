package fi.dy.masa.itemscroller.event;

import org.lwjgl.input.Keyboard;
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
import io.netty.buffer.Unpooled;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.entity.passive.EntityVillager;
import net.minecraft.inventory.ContainerMerchant;
import net.minecraft.inventory.Slot;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.client.CPacketCustomPayload;
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
    public boolean onKeyInput(int eventKey, boolean eventKeyState)
    {
        if (InputUtils.isRecipeViewOpen() && eventKeyState)
        {
            int index = -1;
            RecipeStorage recipes = RecipeStorage.getInstance();
            int oldIndex = recipes.getSelection();
            int recipesPerPage = recipes.getRecipeCountPerPage();
            int recipeIndexChange = GuiScreen.isShiftKeyDown() ? recipesPerPage : recipesPerPage / 2;

            if (eventKey >= Keyboard.KEY_1 && eventKey <= Keyboard.KEY_9)
            {
                index = MathHelper.clamp(eventKey - Keyboard.KEY_1, 0, 8);
            }
            else if (eventKey == Keyboard.KEY_UP && oldIndex > 0)
            {
                index = oldIndex - 1;
            }
            else if (eventKey == Keyboard.KEY_DOWN && oldIndex < (recipes.getTotalRecipeCount() - 1))
            {
                index = oldIndex + 1;
            }
            else if (eventKey == Keyboard.KEY_LEFT && oldIndex >= recipeIndexChange)
            {
                index = oldIndex - recipeIndexChange;
            }
            else if (eventKey == Keyboard.KEY_RIGHT && oldIndex < (recipes.getTotalRecipeCount() - recipeIndexChange))
            {
                index = oldIndex + recipeIndexChange;
            }

            if (index >= 0)
            {
                recipes.changeSelectedRecipe(index);
                return true;
            }
        }

        return this.handleInput(eventKey, eventKeyState, 0);
    }

    @Override
    public boolean onMouseInput(int eventButton, int dWheel, boolean eventButtonState)
    {
        return this.handleInput(eventButton - 100, eventButtonState, dWheel);
    }

    private boolean handleInput(int keyCode, boolean keyState, int dWheel)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (action != MoveAction.NONE && InputUtils.isActionKeyActive(action) == false)
        {
            InventoryUtils.stopDragging();
        }

        Minecraft mc = Minecraft.getMinecraft();
        boolean cancel = false;

        if (this.callbacks.functionalityEnabled() && mc.player != null)
        {
            final boolean isAttack = InputUtils.isAttack(keyCode);
            final boolean isUse = InputUtils.isUse(keyCode);
            final boolean isPickBlock = InputUtils.isPickBlock(keyCode);
            final boolean isAttackUseOrPick = isAttack || isUse || isPickBlock;

            if (Configs.Toggles.VILLAGER_TRADE_LIST.getBooleanValue())
            {
                VillagerDataStorage storage = VillagerDataStorage.getInstance();

                if (mc.currentScreen == null && mc.objectMouseOver != null &&
                    mc.objectMouseOver.typeOfHit == RayTraceResult.Type.ENTITY &&
                    mc.objectMouseOver.entityHit instanceof EntityVillager)
                {
                    storage.setLastInteractedUUID(mc.objectMouseOver.entityHit.getUniqueID());
                }
                else if (mc.currentScreen instanceof GuiMerchant && storage.hasInteractionTarget())
                {
                    WidgetTradeList widget = ((IGuiMerchant) mc.currentScreen).getTradeListWidget();

                    if (widget != null)
                    {
                        final int mouseX = InputUtils.getMouseX();
                        final int mouseY = InputUtils.getMouseY();
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
                        final int mouseX = InputUtils.getMouseX();
                        final int mouseY = InputUtils.getMouseY();
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
                             InputUtils.canShiftDropItems(gui, mc))
                    {
                        cancel |= InventoryUtils.shiftDropItems(gui);
                    }
                }

                if (Configs.Generic.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getBooleanValue())
                {
                    recipes.writeToDisk();
                }
            }
        }

        return cancel;
    }

    @Override
    public void onMouseMoved()
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (this.callbacks.functionalityEnabled() &&
            mc.player != null &&
            mc.currentScreen instanceof GuiContainer &&
            Configs.GUI_BLACKLIST.contains(mc.currentScreen.getClass().getName()) == false)
        {
            this.handleDragging((GuiContainer) mc.currentScreen, mc, false);
        }
    }

    private boolean handleDragging(GuiContainer gui, Minecraft mc, boolean isClick)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (InputUtils.isActionKeyActive(action))
        {
            return InventoryUtils.dragMoveItems(gui, mc, action, false);
        }
        else if (action != MoveAction.NONE)
        {
            InventoryUtils.stopDragging();
        }

        return false;
    }

    public static void changeTradePage(GuiMerchant gui, int page)
    {
        Minecraft mc = Minecraft.getMinecraft();
        MerchantRecipeList trades = gui.getMerchant().getRecipes(mc.player);

        // The trade list is unfortunately synced after the GUI
        // opens, so the trade list can be null here when we want to
        // restore the last viewed page when the GUI first opens
        if (page >= 0 && (trades == null || page < trades.size()))
        {
            ((IGuiMerchant) gui).setSelectedMerchantRecipe(page);
        }

        ((ContainerMerchant) gui.inventorySlots).setCurrentRecipeIndex(page);
        PacketBuffer packetbuffer = new PacketBuffer(Unpooled.buffer());
        packetbuffer.writeInt(page);
        mc.getConnection().sendPacket(new CPacketCustomPayload("MC|TrSel", packetbuffer));
    }
}
