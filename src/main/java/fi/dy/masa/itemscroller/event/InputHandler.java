package fi.dy.masa.itemscroller.event;

import org.lwjgl.glfw.GLFW;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.util.MoveAction;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybindManager;
import fi.dy.masa.malilib.hotkeys.IKeybindProvider;
import fi.dy.masa.malilib.hotkeys.IKeyboardInputHandler;
import fi.dy.masa.malilib.hotkeys.IMouseInputHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.inventory.Slot;
import net.minecraft.util.math.MathHelper;

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
        if (InputUtils.isRecipeViewOpen() && eventKey >= GLFW.GLFW_KEY_1 && eventKey <= GLFW.GLFW_KEY_9)
        {
            int index = MathHelper.clamp(eventKey - GLFW.GLFW_KEY_1, 0, 8);
            this.callbacks.getRecipes().changeSelectedRecipe(index);
            return true;
        }

        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (action != MoveAction.NONE && InputUtils.isActionKeyActive(action) == false)
        {
            InventoryUtils.stopDragging();
        }

        return false;
    }

    @Override
    public boolean onMouseScroll(int mouseX, int mouseY, double amount)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (action != MoveAction.NONE && InputUtils.isActionKeyActive(action) == false)
        {
            InventoryUtils.stopDragging();
        }

        Minecraft mc = Minecraft.getInstance();
        boolean cancel = false;

        if (this.callbacks.functionalityEnabled() &&
            amount != 0 &&
            mc != null &&
            mc.player != null &&
            mc.currentScreen instanceof GuiContainer &&
            (mc.currentScreen instanceof GuiContainerCreative) == false &&
            Configs.GUI_BLACKLIST.contains(mc.currentScreen.getClass().getName()) == false)
        {
            GuiContainer gui = (GuiContainer) mc.currentScreen;
            RecipeStorage recipes = this.callbacks.getRecipes();

            // When scrolling while the recipe view is open, change the selection instead of moving items
            if (InputUtils.isRecipeViewOpen())
            {
                recipes.scrollSelection(amount < 0);
                cancel = true;
            }
            else
            {
                cancel = InventoryUtils.tryMoveItems(gui, recipes, amount > 0);
            }
        }

        return cancel;
    }

    @Override
    public boolean onMouseClick(int mouseX, int mouseY, int eventButton, boolean eventButtonState)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (action != MoveAction.NONE && InputUtils.isActionKeyActive(action) == false)
        {
            InventoryUtils.stopDragging();
        }

        Minecraft mc = Minecraft.getInstance();
        boolean cancel = false;

        if (this.callbacks.functionalityEnabled() &&
            mc != null &&
            mc.player != null &&
            mc.currentScreen instanceof GuiContainer &&
            (mc.currentScreen instanceof GuiContainerCreative) == false &&
            Configs.GUI_BLACKLIST.contains(mc.currentScreen.getClass().getName()) == false)
        {
            GuiContainer gui = (GuiContainer) mc.currentScreen;
            RecipeStorage recipes = this.callbacks.getRecipes();

            Slot slot = AccessorUtils.getSlotUnderMouse(gui);
            final boolean isLeftClick = mc.gameSettings.keyBindAttack.func_197984_a(eventButton);
            final boolean isRightClick = mc.gameSettings.keyBindUseItem.func_197984_a(eventButton);
            final boolean isPickBlock = mc.gameSettings.keyBindPickBlock.func_197984_a(eventButton);
            final boolean isShiftDown = GuiScreen.isShiftKeyDown();

            if (eventButtonState && (isLeftClick || isRightClick || isPickBlock))
            {
                int hoveredRecipeId = RenderEventHandler.instance().getHoveredRecipeId(mouseX, mouseY, recipes, gui, mc);

                // Hovering over an item in the recipe view
                if (hoveredRecipeId >= 0)
                {
                    InventoryUtils.handleRecipeClick(gui, mc, recipes, hoveredRecipeId, isLeftClick, isRightClick, isPickBlock, isShiftDown);
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

            if (eventButtonState && (isLeftClick || isRightClick))
            {
                InventoryUtils.storeSourceSlotCandidate(slot, mc);
            }

            if (Configs.Toggles.RIGHT_CLICK_CRAFT_STACK.getBooleanValue() &&
                isRightClick && eventButtonState &&
                InventoryUtils.isCraftingSlot(gui, slot))
            {
                InventoryUtils.rightClickCraftOneStack(gui);
            }
            else if (Configs.Toggles.SHIFT_PLACE_ITEMS.getBooleanValue() &&
                     isLeftClick && isShiftDown &&
                     InventoryUtils.canShiftPlaceItems(gui))
            {
                cancel |= InventoryUtils.shiftPlaceItems(slot, gui);
            }
            else if (Configs.Toggles.SHIFT_DROP_ITEMS.getBooleanValue() &&
                     isLeftClick && isShiftDown &&
                     InputUtils.canShiftDropItems(gui, mc, mouseX, mouseY))
            {
                cancel |= InventoryUtils.shiftDropItems(gui);
            }

            if (Configs.Generic.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getBooleanValue())
            {
                recipes.writeToDisk();
            }
        }

        return cancel;
    }

    @Override
    public void onMouseMove(int mouseX, int mouseY)
    {
        Minecraft mc = Minecraft.getInstance();

        if (this.callbacks.functionalityEnabled() &&
            mc != null &&
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
}
