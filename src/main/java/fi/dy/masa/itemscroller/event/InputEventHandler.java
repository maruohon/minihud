package fi.dy.masa.itemscroller.event;

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Set;
import org.lwjgl.input.Keyboard;
import org.lwjgl.input.Mouse;
import fi.dy.masa.itemscroller.LiteModItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Configs.Toggles;
import fi.dy.masa.itemscroller.recipes.CraftingHandler;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.init.SoundEvents;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.MathHelper;

public class InputEventHandler
{
    private static final InputEventHandler INSTANCE = new InputEventHandler();
    private boolean disabled;
    private int lastPosX;
    private int lastPosY;
    private int slotNumberLast;
    private final Set<Integer> draggedSlots = new HashSet<Integer>();
    private WeakReference<Slot> sourceSlotCandidate = new WeakReference<Slot>(null);
    private WeakReference<Slot> sourceSlot = new WeakReference<Slot>(null);
    private ItemStack stackInCursorLast = InventoryUtils.EMPTY_STACK;
    private RecipeStorage recipes;

    public InputEventHandler()
    {
        this.initializeRecipeStorage();
    }

    public static InputEventHandler getInstance()
    {
        return INSTANCE;
    }

    public boolean onMouseInput()
    {
        boolean cancel = false;
        Minecraft mc = Minecraft.getMinecraft();
        GuiScreen guiScreen = mc.currentScreen;

        if (this.disabled == false &&
            mc != null &&
            mc.player != null &&
            guiScreen instanceof GuiContainer &&
            Configs.GUI_BLACKLIST.contains(guiScreen.getClass().getName()) == false)
        {
            GuiContainer gui = (GuiContainer) guiScreen;
            int dWheel = Mouse.getEventDWheel();

            // Allow drag moving alone if the GUI is the creative inventory
            if (guiScreen instanceof GuiContainerCreative)
            {
                if (dWheel == 0 &&
                    Configs.Toggles.DRAG_MOVE_SHIFT_LEFT.getValue() ||
                    Configs.Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue() ||
                    Configs.Toggles.DRAG_MOVE_CONTROL_LEFT.getValue())
                {
                    cancel = this.dragMoveItems(gui, mc);
                }

                return cancel;
            }

            if (dWheel != 0)
            {
                // When scrolling while the recipe view is open, change the selection instead of moving items
                if (this.isRecipeViewOpen())
                {
                    this.recipes.scrollSelection(dWheel < 0);
                }
                else
                {
                    cancel = InventoryUtils.tryMoveItems(gui, this.recipes, dWheel > 0);
                }
            }
            else
            {
                Slot slot = AccessorUtils.getSlotUnderMouse(gui);
                boolean isLeftClick = mouseEventIsLeftClick(mc);
                boolean isRightClick = mouseEventIsRightClick(mc);
                boolean isPickBlock = mouseEventIsPickBlock(mc);
                boolean isButtonDown = Mouse.getEventButtonState();

                if (isButtonDown && (isLeftClick || isRightClick || isPickBlock))
                {
                    final int mouseX = RenderEventHandler.instance().getMouseX();
                    final int mouseY = RenderEventHandler.instance().getMouseY();
                    int hoveredRecipeId = RenderEventHandler.instance().getHoveredRecipeId(mouseX, mouseY, this.recipes, gui, mc);

                    // Hovering over an item in the recipe view
                    if (hoveredRecipeId >= 0)
                    {
                        if (isLeftClick || isRightClick)
                        {
                            boolean changed = this.recipes.getSelection() != hoveredRecipeId;
                            this.recipes.changeSelectedRecipe(hoveredRecipeId);

                            if (changed)
                            {
                                InventoryUtils.clearFirstCraftingGridOfItems(this.recipes.getSelectedRecipe(), gui, false);
                            }

                            InventoryUtils.tryMoveItemsToFirstCraftingGrid(this.recipes.getRecipe(hoveredRecipeId), gui, GuiScreen.isShiftKeyDown());

                            // Right click: Also craft the items
                            if (isRightClick)
                            {
                                Slot outputSlot = CraftingHandler.getFirstCraftingOutputSlotForGui(gui);
                                boolean dropKeyDown = isKeybindHeld(mc.gameSettings.keyBindDrop.getKeyCode());

                                if (outputSlot != null)
                                {
                                    if (dropKeyDown)
                                    {
                                        if (GuiScreen.isShiftKeyDown())
                                        {
                                            if (Toggles.CARPET_CTRL_Q_CRAFTING.getValue())
                                            {
                                                InventoryUtils.dropStack(gui, outputSlot.slotNumber);
                                            }
                                            else
                                            {
                                                InventoryUtils.dropStacksUntilEmpty(gui, outputSlot.slotNumber);
                                            }
                                        }
                                        else
                                        {
                                            InventoryUtils.dropItem(gui, outputSlot.slotNumber);
                                        }
                                    }
                                    else
                                    {
                                        if (GuiScreen.isShiftKeyDown())
                                        {
                                            InventoryUtils.shiftClickSlot(gui, outputSlot.slotNumber);
                                        }
                                        else
                                        {
                                            InventoryUtils.moveOneSetOfItemsFromSlotToPlayerInventory(gui, outputSlot);
                                        }
                                    }
                                }
                            }
                        }
                        else if (isPickBlock)
                        {
                            InventoryUtils.clearFirstCraftingGridOfAllItems(gui);
                        }

                        return true;
                    }
                    // Pick-blocking over a crafting output slot with the recipe view open, store the recipe
                    else if (isPickBlock && this.isRecipeViewOpen() && InventoryUtils.isCraftingSlot(gui, slot))
                    {
                        this.recipes.storeCraftingRecipeToCurrentSelection(slot, gui, true);
                        cancel = true;
                    }
                }

                this.checkForItemPickup(gui, mc);
                this.storeSourceSlotCandidate(slot, gui, mc);

                if (Configs.Toggles.RIGHT_CLICK_CRAFT_STACK.getValue() &&
                    isRightClick &&
                    isButtonDown &&
                    InventoryUtils.isCraftingSlot(gui, slot))
                {
                    InventoryUtils.rightClickCraftOneStack(gui);
                }
                else if (Configs.Toggles.SHIFT_PLACE_ITEMS.getValue() && InventoryUtils.canShiftPlaceItems(gui))
                {
                    cancel |= this.shiftPlaceItems(slot, gui);
                }
                else if (Configs.Toggles.SHIFT_DROP_ITEMS.getValue() && this.canShiftDropItems(gui, mc))
                {
                    cancel |= this.shiftDropItems(gui);
                }
                else if (Configs.Toggles.ALT_SHIFT_CLICK_EVERYTHING.getValue() &&
                         isLeftClick &&
                         isButtonDown &&
                         GuiScreen.isAltKeyDown() &&
                         GuiScreen.isShiftKeyDown() &&
                         slot != null && InventoryUtils.isStackEmpty(slot.getStack()) == false)
                {
                    InventoryUtils.tryMoveStacks(slot, gui, false, true, false);
                    cancel = true;
                }
                else if (Configs.Toggles.ALT_CLICK_MATCHING.getValue() &&
                         isLeftClick &&
                         Mouse.getEventButtonState() &&
                         GuiScreen.isAltKeyDown() &&
                         slot != null && InventoryUtils.isStackEmpty(slot.getStack()) == false)
                {
                    InventoryUtils.tryMoveStacks(slot, gui, true, true, false);
                    cancel = true;
                }
                else if (Mouse.getEventButtonState() && Mouse.getEventButton() == 0 && this.shouldMoveVertically())
                {
                    MoveType type = this.getDragMoveType(mc);
                    MoveAmount amount = this.getDragMoveAmount(type, mc);
                    InventoryUtils.tryMoveItemsVertically(gui, slot, this.recipes, Keyboard.isKeyDown(Keyboard.KEY_W), amount);
                    this.slotNumberLast = -1;
                    cancel = true;
                }
                else if (Configs.Toggles.DRAG_MOVE_SHIFT_LEFT.getValue() ||
                         Configs.Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue() ||
                         Configs.Toggles.DRAG_MOVE_CONTROL_LEFT.getValue() ||
                         Configs.Toggles.DRAG_DROP_SINGLE.getValue() ||
                         Configs.Toggles.DRAG_DROP_STACKS.getValue())
               {
                   cancel |= this.dragMoveItems(gui, mc);
               }
            }

            if (Configs.Toggles.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getValue())
            {
                this.recipes.writeToDisk();
            }
        }

        return cancel;
    }

    public boolean onKeyInput()
    {
        Minecraft mc = Minecraft.getMinecraft();
        GuiScreen guiScreen = mc.currentScreen;

        if (mc == null || mc.player == null || (guiScreen instanceof GuiContainer) == false || Keyboard.getEventKeyState() == false)
        {
            return false;
        }

        final int eventKey = Keyboard.getEventKey();
        GuiContainer gui = (GuiContainer) guiScreen;
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);

        // Swap the hovered stack to the Offhand
        if (Configs.Toggles.OFFHAND_SWAP.getValue() &&
            eventKey == mc.gameSettings.keyBindSwapHands.getKeyCode() &&
            (gui instanceof GuiInventory) && slot != null)
        {
            InventoryUtils.swapSlots(gui, slot.slotNumber, 45);
        }
        else if (GuiScreen.isAltKeyDown() &&
            GuiScreen.isShiftKeyDown() &&
            GuiScreen.isCtrlKeyDown())
        {
            if (eventKey == Keyboard.KEY_C)
            {
                InventoryUtils.craftEverythingPossibleWithCurrentRecipe(this.recipes.getSelectedRecipe(), gui);
            }
            else if (eventKey == Keyboard.KEY_T)
            {
                InventoryUtils.throwAllCraftingResultsToGround(this.recipes.getSelectedRecipe(), gui);
            }
            else if (eventKey == Keyboard.KEY_M)
            {
                InventoryUtils.moveAllCraftingResultsToOtherInventory(this.recipes.getSelectedRecipe(), gui);
            }
            else if (eventKey == Keyboard.KEY_I)
            {
                if (slot != null)
                {
                    debugPrintSlotInfo(gui, slot);
                }
                else
                {
                    LiteModItemScroller.logger.info("GUI class: {}", gui.getClass().getName());
                }
            }
        }

        // Drop all matching stacks from the same inventory when pressing Ctrl + Shift + Drop key
        if (Configs.Toggles.CONTROL_SHIFT_DROP.getValue() &&
            Configs.GUI_BLACKLIST.contains(gui.getClass().getName()) == false &&
            GuiScreen.isCtrlKeyDown() && GuiScreen.isShiftKeyDown() &&
            eventKey == mc.gameSettings.keyBindDrop.getKeyCode())
        {
            if (slot != null && slot.getHasStack())
            {
                InventoryUtils.dropStacks(gui, slot.getStack(), slot, true);
            }
        }
        // Toggle mouse functionality on/off
        else if (eventKey == LiteModItemScroller.KEY_DISABLE.getKeyCode())
        {
            this.disabled = ! this.disabled;

            if (this.disabled)
            {
                mc.player.playSound(SoundEvents.BLOCK_NOTE_BASS, 0.8f, 0.8f);
            }
            else
            {
                mc.player.playSound(SoundEvents.BLOCK_NOTE_PLING, 0.5f, 1.0f);
            }
        }
        // Store or load a recipe
        else if (this.isRecipeViewOpen() && eventKey >= Keyboard.KEY_1 && eventKey <= Keyboard.KEY_9)
        {
            int index = MathHelper.clamp(eventKey - Keyboard.KEY_1, 0, 8);
            this.recipes.changeSelectedRecipe(index);
            return true;
        }

        return false;
    }

    public void onWorldChanged()
    {
        if (Configs.Toggles.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getValue())
        {
            this.recipes.readFromDisk();
        }
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

    public boolean isRecipeViewOpen()
    {
        int keyCode = LiteModItemScroller.KEY_RECIPE.getKeyCode();
        return isKeybindHeld(keyCode) && GuiScreen.isCtrlKeyDown() == false && GuiScreen.isAltKeyDown() == false;
    }

    public void initializeRecipeStorage()
    {
        this.recipes = new RecipeStorage(18, Configs.Toggles.SCROLL_CRAFT_RECIPE_FILE_GLOBAL.getValue());
    }

    public RecipeStorage getRecipes()
    {
        return this.recipes;
    }

    /**
     * Store a reference to the slot when a slot is left or right clicked on.
     * The slot is then later used to determine which inventory an ItemStack was
     * picked up from, if the stack from the cursor is dropped while holding shift.
     */
    private void storeSourceSlotCandidate(Slot slot, GuiContainer gui, Minecraft mc)
    {
        // Left or right mouse button was pressed
        if (slot != null && Mouse.getEventButtonState() && (mouseEventIsLeftClick(mc) || mouseEventIsRightClick(mc)))
        {
            ItemStack stackCursor = mc.player.inventory.getItemStack();
            ItemStack stack = InventoryUtils.EMPTY_STACK;

            if (InventoryUtils.isStackEmpty(stackCursor) == false)
            {
                // Do a cheap copy without NBT data
                stack = new ItemStack(stackCursor.getItem(), InventoryUtils.getStackSize(stackCursor), stackCursor.getMetadata());
            }

            this.stackInCursorLast = stack;
            this.sourceSlotCandidate = new WeakReference<Slot>(slot);
        }
    }

    /**
     * Check if the (previous) mouse event resulted in picking up a new ItemStack to the cursor
     */
    private void checkForItemPickup(GuiContainer gui, Minecraft mc)
    {
        ItemStack stackCursor = mc.player.inventory.getItemStack();

        // Picked up or swapped items to the cursor, grab a reference to the slot that the items came from
        // Note that we are only checking the item and metadata here!
        if (InventoryUtils.isStackEmpty(stackCursor) == false && stackCursor.isItemEqual(this.stackInCursorLast) == false)
        {
            this.sourceSlot = new WeakReference<Slot>(this.sourceSlotCandidate.get());
        }
    }

    private static void debugPrintSlotInfo(GuiContainer gui, Slot slot)
    {
        if (slot == null)
        {
            LiteModItemScroller.logger.info("slot was null");
            return;
        }

        boolean hasSlot = gui.inventorySlots.inventorySlots.contains(slot);
        Object inv = slot.inventory;
        String stackStr = InventoryUtils.getStackString(slot.getStack());

        LiteModItemScroller.logger.info(String.format("slot: slotNumber: %d, getSlotIndex(): %d, getHasStack(): %s, " +
                "slot class: %s, inv class: %s, Container's slot list has slot: %s, stack: %s, numSlots: %d",
                slot.slotNumber, AccessorUtils.getSlotIndex(slot), slot.getHasStack(), slot.getClass().getName(),
                inv != null ? inv.getClass().getName() : "<null>", hasSlot ? " true" : "false", stackStr,
                gui.inventorySlots.inventorySlots.size()));
    }

    private boolean shiftPlaceItems(Slot slot, GuiContainer gui)
    {
        // Left click to place the items from the cursor to the slot
        InventoryUtils.leftClickSlot(gui, slot.slotNumber);

        // Ugly fix to prevent accidentally drag-moving the stack from the slot that it was just placed into...
        this.draggedSlots.add(slot.slotNumber);

        InventoryUtils.tryMoveStacks(slot, gui, true, false, false);

        return true;
    }

    private boolean shiftDropItems(GuiContainer gui)
    {
        ItemStack stackReference = Minecraft.getMinecraft().player.inventory.getItemStack();

        if (InventoryUtils.isStackEmpty(stackReference) == false)
        {
            stackReference = stackReference.copy();

            // First drop the existing stack from the cursor
            InventoryUtils.dropItemsFromCursor(gui);

            InventoryUtils.dropStacks(gui, stackReference, this.sourceSlot.get(), true);
            return true;
        }

        return false;
    }

    private boolean canShiftDropItems(GuiContainer gui, Minecraft mc)
    {
        if (GuiScreen.isShiftKeyDown() && mouseEventIsLeftClick(mc) &&
            InventoryUtils.isStackEmpty(mc.player.inventory.getItemStack()) == false)
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

    private boolean dragMoveItems(GuiContainer gui, Minecraft mc)
    {
        int mouseX = Mouse.getEventX() * gui.width / mc.displayWidth;
        int mouseY = gui.height - Mouse.getEventY() * gui.height / mc.displayHeight - 1;

        if (InventoryUtils.isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // Updating these here is part of the fix to preventing a drag after shift + place
            this.lastPosX = mouseX;
            this.lastPosY = mouseY;
            return false;
        }

        MoveType type = this.getDragMoveType(mc);
        MoveAmount amount = this.getDragMoveAmount(type, mc);
        boolean cancel = false;

        if (Mouse.getEventButtonState())
        {
            // Reset this or the method call won't do anything...
            this.slotNumberLast = -1;
            cancel = this.dragMoveFromSlotAtPosition(gui, mouseX, mouseY, type, amount);
        }

        if (cancel == false)
        {
            int distX = mouseX - this.lastPosX;
            int distY = mouseY - this.lastPosY;
            int absX = Math.abs(distX);
            int absY = Math.abs(distY);

            if (absX > absY)
            {
                int inc = distX > 0 ? 1 : -1;

                for (int x = this.lastPosX; ; x += inc)
                {
                    int y = absX != 0 ? this.lastPosY + ((x - this.lastPosX) * distY / absX) : mouseY;
                    this.dragMoveFromSlotAtPosition(gui, x, y, type, amount);

                    if (x == mouseX)
                    {
                        break;
                    }
                }
            }
            else
            {
                int inc = distY > 0 ? 1 : -1;

                for (int y = this.lastPosY; ; y += inc)
                {
                    int x = absY != 0 ? this.lastPosX + ((y - this.lastPosY) * distX / absY) : mouseX;
                    this.dragMoveFromSlotAtPosition(gui, x, y, type, amount);

                    if (y == mouseY)
                    {
                        break;
                    }
                }
            }
        }

        this.lastPosX = mouseX;
        this.lastPosY = mouseY;

        // Always update the slot under the mouse.
        // This should prevent a "double click/move" when shift + left clicking on slots that have more
        // than one stack of items. (the regular slotClick() + a "drag move" from the slot that is under the mouse
        // when the left mouse button is pressed down and this code runs).
        Slot slot = AccessorUtils.getSlotAtPosition(gui, mouseX, mouseY);

        if (slot != null)
        {
            if (gui instanceof GuiContainerCreative)
            {
                boolean isPlayerInv = ((GuiContainerCreative) gui).getSelectedTabIndex() == CreativeTabs.INVENTORY.getTabIndex();
                int slotNumber = isPlayerInv ? AccessorUtils.getSlotIndex(slot) : slot.slotNumber;
                this.slotNumberLast = slotNumber;
            }
            else
            {
                this.slotNumberLast = slot.slotNumber;
            }
        }
        else
        {
            this.slotNumberLast = -1;
        }

        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);

        if (leftButtonDown == false && rightButtonDown == false)
        {
            this.draggedSlots.clear();
        }

        return cancel;
    }

    private MoveType getDragMoveType(Minecraft mc)
    {
        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);
        boolean isShiftDown = GuiScreen.isShiftKeyDown();
        boolean isControlDown = GuiScreen.isCtrlKeyDown();
        boolean eitherMouseButtonDown = leftButtonDown || rightButtonDown;

        // Only one of the mouse buttons is down, and only one of shift or control is down
        if (leftButtonDown ^ rightButtonDown)
        {
            if (this.shouldMoveVertically())
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
                    ((isShiftDown && Toggles.DRAG_DROP_STACKS.getValue()) ||
                     (isControlDown && Toggles.DRAG_DROP_SINGLE.getValue())))
                {
                    return MoveType.DROP;
                }
                else if ((isShiftDown && leftButtonDown && Toggles.DRAG_MOVE_SHIFT_LEFT.getValue()) ||
                    (isShiftDown && rightButtonDown && Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue()) ||
                    (isControlDown && eitherMouseButtonDown && Toggles.DRAG_MOVE_CONTROL_LEFT.getValue()))
                {
                    return MoveType.MOVE_TO_OTHER;
                }
            }
        }

        return MoveType.INVALID;
    }

    private MoveAmount getDragMoveAmount(MoveType type, Minecraft mc)
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

    private boolean dragMoveFromSlotAtPosition(GuiContainer gui, int x, int y, MoveType type, MoveAmount amount)
    {
        if (gui instanceof GuiContainerCreative)
        {
            return this.dragMoveFromSlotAtPositionCreative(gui, x, y, type, amount);
        }

        Slot slot = AccessorUtils.getSlotAtPosition(gui, x, y);
        Minecraft mc = Minecraft.getMinecraft();
        boolean flag = slot != null && InventoryUtils.isValidSlot(slot, gui, true) && slot.canTakeStack(mc.player);
        boolean cancel = flag && (amount == MoveAmount.LEAVE_ONE || amount == MoveAmount.MOVE_ONE);

        if (flag && slot.slotNumber != this.slotNumberLast &&
            (amount != MoveAmount.MOVE_ONE || this.draggedSlots.contains(slot.slotNumber) == false))
        {
            if (type == MoveType.MOVE_TO_OTHER)
            {
                if (amount == MoveAmount.MOVE_ONE)
                {
                    InventoryUtils.tryMoveSingleItemToOtherInventory(slot, gui);
                }
                else if (amount == MoveAmount.LEAVE_ONE)
                {
                    InventoryUtils.tryMoveAllButOneItemToOtherInventory(slot, gui);
                }
                else
                {
                    InventoryUtils.shiftClickSlot(gui, slot.slotNumber);
                    cancel = true;
                }
            }
            else if (type == MoveType.MOVE_UP || type == MoveType.MOVE_DOWN)
            {
                InventoryUtils.tryMoveItemsVertically(gui, slot, this.recipes, Keyboard.isKeyDown(Keyboard.KEY_W), amount);
                cancel = true;
            }
            else if (type == MoveType.DROP)
            {
                if (amount == MoveAmount.MOVE_ONE)
                {
                    InventoryUtils.clickSlot(gui, slot.slotNumber, 0, ClickType.THROW);
                }
                else if (amount == MoveAmount.LEAVE_ONE)
                {
                    InventoryUtils.leftClickSlot(gui, slot.slotNumber);
                    InventoryUtils.rightClickSlot(gui, slot.slotNumber);
                    InventoryUtils.dropItemsFromCursor(gui);
                }
                else
                {
                    InventoryUtils.clickSlot(gui, slot.slotNumber, 1, ClickType.THROW);
                    cancel = true;
                }
            }

            this.draggedSlots.add(slot.slotNumber);
        }

        return cancel;
    }

    private boolean dragMoveFromSlotAtPositionCreative(GuiContainer gui, int x, int y, MoveType type, MoveAmount amount)
    {
        GuiContainerCreative guiCreative = (GuiContainerCreative) gui;
        Slot slot = AccessorUtils.getSlotAtPosition(gui, x, y);
        boolean isPlayerInv = guiCreative.getSelectedTabIndex() == CreativeTabs.INVENTORY.getTabIndex();

        // Only allow dragging from the hotbar slots
        if (slot == null || (slot.getClass() != Slot.class && isPlayerInv == false))
        {
            return false;
        }

        Minecraft mc = Minecraft.getMinecraft();
        boolean flag = slot != null && InventoryUtils.isValidSlot(slot, gui, true) && slot.canTakeStack(mc.player);
        boolean cancel = flag && (amount == MoveAmount.LEAVE_ONE || amount == MoveAmount.MOVE_ONE);
        // The player inventory tab of the creative inventory uses stupid wrapped
        // slots that all have slotNumber = 0 on the outer instance ;_;
        // However in that case we can use the slotIndex which is easy enough to get.
        int slotNumber = isPlayerInv ? AccessorUtils.getSlotIndex(slot) : slot.slotNumber;

        if (flag && slotNumber != this.slotNumberLast && this.draggedSlots.contains(slotNumber) == false)
        {
            if (type == MoveType.MOVE_TO_OTHER)
            {
                if (amount == MoveAmount.MOVE_ONE)
                {
                    this.leftClickSlot(guiCreative, slot, slotNumber);
                    this.rightClickSlot(guiCreative, slot, slotNumber);
                    this.shiftClickSlot(guiCreative, slot, slotNumber);
                    this.leftClickSlot(guiCreative, slot, slotNumber);

                    cancel = true;
                }
                else if (amount == MoveAmount.LEAVE_ONE)
                {
                    // Too lazy to try to duplicate the proper code for the weird creative inventory...
                    if (isPlayerInv == false)
                    {
                        this.leftClickSlot(guiCreative, slot, slotNumber);
                        this.rightClickSlot(guiCreative, slot, slotNumber);

                        // Delete the rest of the stack by placing it in the first creative "source slot"
                        Slot slotFirst = gui.inventorySlots.inventorySlots.get(0);
                        this.leftClickSlot(guiCreative, slotFirst, slotFirst.slotNumber);
                    }

                    cancel = true;
                }
                else
                {
                    this.shiftClickSlot(gui, slot, slotNumber);
                    cancel = true;
                }
            }
            else if (type == MoveType.DROP)
            {
                if (amount == MoveAmount.MOVE_ONE)
                {
                    InventoryUtils.clickSlot(gui, slot.slotNumber, 0, ClickType.THROW);
                }
                else if (amount == MoveAmount.LEAVE_ONE)
                {
                    InventoryUtils.leftClickSlot(gui, slot.slotNumber);
                    InventoryUtils.rightClickSlot(gui, slot.slotNumber);
                    InventoryUtils.dropItemsFromCursor(gui);
                }
                else
                {
                    InventoryUtils.clickSlot(gui, slot.slotNumber, 1, ClickType.THROW);
                    cancel = true;
                }
            }

            this.draggedSlots.add(slotNumber);
        }

        return cancel;
    }

    private boolean shouldMoveVertically()
    {
        return Toggles.WS_CLICKING.getValue() && (Keyboard.isKeyDown(Keyboard.KEY_W) || Keyboard.isKeyDown(Keyboard.KEY_S));
    }

    public static boolean mouseEventIsLeftClick(Minecraft mc)
    {
        return Mouse.getEventButton() == mc.gameSettings.keyBindAttack.getKeyCode() + 100;
    }

    public static boolean mouseEventIsRightClick(Minecraft mc)
    {
        return Mouse.getEventButton() == mc.gameSettings.keyBindUseItem.getKeyCode() + 100;
    }

    public static boolean mouseEventIsPickBlock(Minecraft mc)
    {
        return Mouse.getEventButton() == mc.gameSettings.keyBindPickBlock.getKeyCode() + 100;
    }

    private void leftClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        InventoryUtils.clickSlot(gui, slot, slotNumber, 0, ClickType.PICKUP);
    }

    private void rightClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        InventoryUtils.clickSlot(gui, slot, slotNumber, 1, ClickType.PICKUP);
    }

    private void shiftClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        InventoryUtils.clickSlot(gui, slot, slotNumber, 0, ClickType.QUICK_MOVE);
    }

    public enum MoveAmount
    {
        INVALID,
        MOVE_ONE,
        MOVE_ALL,
        LEAVE_ONE
    }

    public enum MoveType
    {
        INVALID,
        MOVE_TO_OTHER,
        MOVE_TO_THIS,
        MOVE_UP,
        MOVE_DOWN,
        DROP
    }
}
