package fi.dy.masa.itemscroller.event;

import org.lwjgl.glfw.GLFW;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.screen.ingame.CreativeInventoryScreen;
import net.minecraft.client.gui.screen.ingame.HandledScreen;
import net.minecraft.entity.passive.MerchantEntity;
import net.minecraft.screen.slot.Slot;
import net.minecraft.util.hit.EntityHitResult;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.MathHelper;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.ClickPacketBuffer;
import fi.dy.masa.itemscroller.util.InputUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.util.MoveAction;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybindManager;
import fi.dy.masa.malilib.hotkeys.IKeybindProvider;
import fi.dy.masa.malilib.hotkeys.IKeyboardInputHandler;
import fi.dy.masa.malilib.hotkeys.IMouseInputHandler;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.malilib.util.KeyCodes;

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
            int recipeIndexChange = GuiBase.isShiftDown() ? recipesPerPage : recipesPerPage / 2;

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
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc.player == null)
        {
            return false;
        }

        if (Configs.Generic.RATE_LIMIT_CLICK_PACKETS.getBooleanValue() &&
            this.callbacks.functionalityEnabled())
        {
            ClickPacketBuffer.setShouldBufferClickPackets(true);
        }

        boolean cancel = this.handleInputImpl(keyCode, keyState, dWheel, mc);

        ClickPacketBuffer.setShouldBufferClickPackets(false);

        return cancel;
    }

    private boolean handleInputImpl(int keyCode, boolean keyState, double dWheel, MinecraftClient mc)
    {
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (action != MoveAction.NONE && InputUtils.isActionKeyActive(action) == false)
        {
            InventoryUtils.stopDragging();
        }

        boolean cancel = false;

        if (this.callbacks.functionalityEnabled() && mc.player != null)
        {
            final boolean isAttack = InputUtils.isAttack(keyCode);
            final boolean isUse = InputUtils.isUse(keyCode);
            final boolean isPickBlock = InputUtils.isPickBlock(keyCode);
            final boolean isAttackUseOrPick = isAttack || isUse || isPickBlock;
            final int mouseX = fi.dy.masa.malilib.util.InputUtils.getMouseX();
            final int mouseY = fi.dy.masa.malilib.util.InputUtils.getMouseY();
            Screen screen = GuiUtils.getCurrentScreen();

            if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue())
            {
                VillagerDataStorage storage = VillagerDataStorage.getInstance();

                if (screen == null && mc.crosshairTarget != null &&
                    mc.crosshairTarget.getType() == HitResult.Type.ENTITY &&
                    ((EntityHitResult) mc.crosshairTarget).getEntity() instanceof MerchantEntity)
                {
                    storage.setLastInteractedUUID(((EntityHitResult) mc.crosshairTarget).getEntity().getUuid());
                }
            }

            if (screen instanceof HandledScreen &&
                (screen instanceof CreativeInventoryScreen) == false &&
                Configs.GUI_BLACKLIST.contains(screen.getClass().getName()) == false)
            {
                HandledScreen<?> gui = (HandledScreen<?>) screen;
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
                    final boolean isShiftDown = GuiBase.isShiftDown();

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

                    InventoryUtils.checkForItemPickup(gui);

                    if (keyState && (isAttack || isUse))
                    {
                        InventoryUtils.storeSourceSlotCandidate(slot, gui);
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
        MinecraftClient mc = MinecraftClient.getInstance();

        if (this.callbacks.functionalityEnabled() &&
            mc.player != null &&
            GuiUtils.getCurrentScreen() instanceof HandledScreen screen &&
            Configs.GUI_BLACKLIST.contains(screen.getClass().getName()) == false)
        {
            this.handleDragging(screen, mc, mouseX, mouseY, false);
        }
    }

    private boolean handleDragging(HandledScreen<?> gui, MinecraftClient mc, int mouseX, int mouseY, boolean isClick)
    {
        boolean cancel = false;
        MoveAction action = InventoryUtils.getActiveMoveAction();

        if (Configs.Generic.RATE_LIMIT_CLICK_PACKETS.getBooleanValue())
        {
            ClickPacketBuffer.setShouldBufferClickPackets(true);
        }

        if (InputUtils.isActionKeyActive(action))
        {
            cancel = InventoryUtils.dragMoveItems(gui, action, mouseX, mouseY, false);
        }
        else if (action != MoveAction.NONE)
        {
            InventoryUtils.stopDragging();
        }

        ClickPacketBuffer.setShouldBufferClickPackets(false);

        return cancel;
    }
}
