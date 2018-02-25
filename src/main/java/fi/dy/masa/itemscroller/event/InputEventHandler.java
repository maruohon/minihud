package fi.dy.masa.itemscroller.event;

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Set;
import org.lwjgl.input.Keyboard;
import org.lwjgl.input.Mouse;
import fi.dy.masa.itemscroller.LiteModItemScroller;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.AccessorUtils;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiContainerCreative;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.init.SoundEvents;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.MathHelper;

public class InputEventHandler
{
    private static InputEventHandler instance;
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

    public static InputEventHandler instance()
    {
        if (instance == null)
        {
            instance = new InputEventHandler();
        }

        return instance;
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
            GuiContainer guiContainer = (GuiContainer) guiScreen;
            int dWheel = Mouse.getEventDWheel();

            // Allow drag moving alone if the GUI is the creative inventory
            if (guiScreen instanceof GuiContainerCreative)
            {
                if (dWheel == 0 &&
                    Configs.Toggles.DRAG_MOVE_SHIFT_LEFT.getValue() ||
                    Configs.Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue() ||
                    Configs.Toggles.DRAG_MOVE_CONTROL_LEFT.getValue())
                {
                    cancel = this.dragMoveItems(guiContainer);
                }

                return cancel;
            }

            if (dWheel != 0)
            {
                // When scrolling while the recipe view is open, change the selection instead of moving items
                if (RenderEventHandler.getRenderStoredRecipes())
                {
                    this.recipes.scrollSelection(dWheel < 0);
                }
                else
                {
                    cancel = InventoryUtils.tryMoveItems(guiContainer, this.recipes, dWheel > 0);
                }
            }
            else
            {
                Slot slot = AccessorUtils.getSlotUnderMouse(guiContainer);
                this.checkForItemPickup(guiContainer);
                this.storeSourceSlotCandidate(guiContainer);

                if (Configs.Toggles.RIGHT_CLICK_CRAFT_STACK.getValue() && Mouse.getEventButton() == 1 &&
                    InventoryUtils.isCraftingSlot(guiContainer, AccessorUtils.getSlotUnderMouse(guiContainer)))
                {
                    InventoryUtils.rightClickCraftOneStack(guiContainer);
                }
                else if (Configs.Toggles.SHIFT_PLACE_ITEMS.getValue() && InventoryUtils.canShiftPlaceItems(guiContainer))
                {
                    cancel = this.shiftPlaceItems(guiContainer);
                }
                else if (Configs.Toggles.SHIFT_DROP_ITEMS.getValue() && this.canShiftDropItems(guiContainer))
                {
                    cancel = this.shiftDropItems(guiContainer);
                }
                else if (Configs.Toggles.ALT_SHIFT_CLICK_EVERYTHING.getValue() &&
                         GuiScreen.isAltKeyDown() &&
                         GuiScreen.isShiftKeyDown() &&
                         Mouse.getEventButtonState() &&
                         Mouse.getEventButton() == mc.gameSettings.keyBindAttack.getKeyCode() + 100 &&
                         slot != null && InventoryUtils.isStackEmpty(slot.getStack()) == false)
                {
                    InventoryUtils.tryMoveStacks(slot, guiContainer, false, true, false);
                    cancel = true;
                }
                else if (Configs.Toggles.ALT_CLICK_MATCHING.getValue() &&
                         GuiScreen.isAltKeyDown() &&
                         Mouse.getEventButtonState() &&
                         Mouse.getEventButton() == mc.gameSettings.keyBindAttack.getKeyCode() + 100 &&
                         slot != null && InventoryUtils.isStackEmpty(slot.getStack()) == false)
                {
                    InventoryUtils.tryMoveStacks(slot, guiContainer, true, true, false);
                    cancel = true;
                }
                else if (Configs.Toggles.DRAG_MOVE_SHIFT_LEFT.getValue() ||
                         Configs.Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue() ||
                         Configs.Toggles.DRAG_MOVE_CONTROL_LEFT.getValue())
                {
                    cancel = this.dragMoveItems(guiContainer);
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

        if (mc == null || mc.player == null || (guiScreen instanceof GuiContainer) == false)
        {
            return false;
        }

        GuiContainer gui = (GuiContainer) guiScreen;
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);

        if (Keyboard.getEventKey() == Keyboard.KEY_I && Keyboard.getEventKeyState() &&
            GuiScreen.isAltKeyDown() && GuiScreen.isCtrlKeyDown() && GuiScreen.isShiftKeyDown())
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
        // Drop all matching stacks from the same inventory when pressing Ctrl + Shift + Drop key
        else if (Configs.Toggles.CONTROL_SHIFT_DROP.getValue() && Keyboard.getEventKeyState() &&
            Configs.GUI_BLACKLIST.contains(gui.getClass().getName()) == false &&
            GuiScreen.isCtrlKeyDown() && GuiScreen.isShiftKeyDown() &&
            mc.gameSettings.keyBindDrop.getKeyCode() == Keyboard.getEventKey())
        {
            if (slot != null && slot.getHasStack())
            {
                InventoryUtils.dropStacks(gui, slot.getStack(), slot);
            }
        }
        // Toggle mouse functionality on/off
        else if (Keyboard.getEventKeyState() && LiteModItemScroller.KEY_DISABLE.getKeyCode() == Keyboard.getEventKey())
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
        // Show or hide the recipe selection
        else if (Keyboard.getEventKey() == LiteModItemScroller.KEY_RECIPE.getKeyCode())
        {
            if (Keyboard.getEventKeyState())
            {
                RenderEventHandler.setRenderStoredRecipes(true);
            }
            else
            {
                RenderEventHandler.setRenderStoredRecipes(false);
            }
        }
        // Store or load a recipe
        else if (Keyboard.getEventKeyState() && Keyboard.isKeyDown(LiteModItemScroller.KEY_RECIPE.getKeyCode()) &&
                 Keyboard.getEventKey() >= Keyboard.KEY_1 && Keyboard.getEventKey() <= Keyboard.KEY_9)
        {
            int index = MathHelper.clamp(Keyboard.getEventKey() - Keyboard.KEY_1, 0, 8);
            InventoryUtils.storeOrLoadRecipe(gui, index);
            //event.setCanceled(true);
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
    private void storeSourceSlotCandidate(GuiContainer gui)
    {
        // Left or right mouse button was pressed
        if (Mouse.getEventButtonState() && (Mouse.getEventButton() == 0 || Mouse.getEventButton() == 1))
        {
            Slot slot = AccessorUtils.getSlotUnderMouse(gui);

            if (slot != null)
            {
                Minecraft mc = Minecraft.getMinecraft();
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
    }

    /**
     * Check if the (previous) mouse event resulted in picking up a new ItemStack to the cursor
     */
    private void checkForItemPickup(GuiContainer gui)
    {
        Minecraft mc = Minecraft.getMinecraft();
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

    private boolean shiftPlaceItems(GuiContainer gui)
    {
        Slot slot = AccessorUtils.getSlotUnderMouse(gui);

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

            InventoryUtils.dropStacks(gui, stackReference, this.sourceSlot.get());
            return true;
        }

        return false;
    }

    private boolean canShiftDropItems(GuiContainer gui)
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (GuiScreen.isShiftKeyDown() == false || Mouse.getEventButton() != 0 ||
            InventoryUtils.isStackEmpty(mc.player.inventory.getItemStack()))
        {
            return false;
        }

        int left = AccessorUtils.getGuiLeft(gui);
        int top = AccessorUtils.getGuiTop(gui);
        int xSize = AccessorUtils.getGuiXSize(gui);
        int ySize = AccessorUtils.getGuiYSize(gui);
        int mouseAbsX = Mouse.getEventX() * gui.width / mc.displayWidth;
        int mouseAbsY = gui.height - Mouse.getEventY() * gui.height / mc.displayHeight - 1;
        boolean isOutsideGui = mouseAbsX < left || mouseAbsY < top || mouseAbsX >= left + xSize || mouseAbsY >= top + ySize;

        return isOutsideGui && AccessorUtils.getSlotAtPosition(gui, mouseAbsX - left, mouseAbsY - top) == null;
    }

    private boolean dragMoveItems(GuiContainer gui)
    {
        Minecraft mc = Minecraft.getMinecraft();
        int mouseX = Mouse.getEventX() * gui.width / mc.displayWidth;
        int mouseY = gui.height - Mouse.getEventY() * gui.height / mc.displayHeight - 1;

        if (InventoryUtils.isStackEmpty(mc.player.inventory.getItemStack()) == false)
        {
            // Updating these here is part of the fix to preventing a drag after shift + place
            this.lastPosX = mouseX;
            this.lastPosY = mouseY;
            return false;
        }

        boolean eventKeyIsLeftButton = Mouse.getEventButton() == 0;
        boolean eventKeyIsRightButton = Mouse.getEventButton() == 1;
        boolean leftButtonDown = Mouse.isButtonDown(0);
        boolean rightButtonDown = Mouse.isButtonDown(1);
        boolean isShiftDown = GuiScreen.isShiftKeyDown();
        boolean isControlDown = GuiScreen.isCtrlKeyDown();
        boolean eitherMouseButtonDown = leftButtonDown || rightButtonDown;

        if ((isShiftDown && leftButtonDown && Configs.Toggles.DRAG_MOVE_SHIFT_LEFT.getValue() == false) ||
            (isShiftDown && rightButtonDown && Configs.Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue() == false) ||
            (isControlDown && eitherMouseButtonDown && Configs.Toggles.DRAG_MOVE_CONTROL_LEFT.getValue() == false))
        {
            return false;
        }

        boolean leaveOneItem = leftButtonDown == false;
        boolean moveOnlyOne = isShiftDown == false;
        boolean cancel = false;

        if (Mouse.getEventButtonState())
        {
            if (((eventKeyIsLeftButton || eventKeyIsRightButton) && isControlDown && Configs.Toggles.DRAG_MOVE_CONTROL_LEFT.getValue()) ||
                (eventKeyIsRightButton && isShiftDown && Configs.Toggles.DRAG_MOVE_SHIFT_RIGHT.getValue()))
            {
                // Reset this or the method call won't do anything...
                this.slotNumberLast = -1;
                cancel = this.dragMoveFromSlotAtPosition(gui, mouseX, mouseY, leaveOneItem, moveOnlyOne);
            }
        }

        // Check that either mouse button is down
        if (cancel == false && (isShiftDown || isControlDown) && eitherMouseButtonDown)
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
                    this.dragMoveFromSlotAtPosition(gui, x, y, leaveOneItem, moveOnlyOne);

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
                    this.dragMoveFromSlotAtPosition(gui, x, y, leaveOneItem, moveOnlyOne);

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

        if (eitherMouseButtonDown == false)
        {
            this.draggedSlots.clear();
        }

        return cancel;
    }

    private boolean dragMoveFromSlotAtPosition(GuiContainer gui, int x, int y, boolean leaveOneItem, boolean moveOnlyOne)
    {
        if (gui instanceof GuiContainerCreative)
        {
            return this.dragMoveFromSlotAtPositionCreative(gui, x, y, leaveOneItem, moveOnlyOne);
        }

        Slot slot = AccessorUtils.getSlotAtPosition(gui, x, y);
        Minecraft mc = Minecraft.getMinecraft();
        boolean flag = slot != null && InventoryUtils.isValidSlot(slot, gui, true) && slot.canTakeStack(mc.player);
        boolean cancel = flag && (leaveOneItem || moveOnlyOne);

        if (flag && slot.slotNumber != this.slotNumberLast && this.draggedSlots.contains(slot.slotNumber) == false)
        {
            if (moveOnlyOne)
            {
                cancel = InventoryUtils.tryMoveSingleItemToOtherInventory(slot, gui);
            }
            else if (leaveOneItem)
            {
                cancel = InventoryUtils.tryMoveAllButOneItemToOtherInventory(slot, gui);
            }
            else
            {
                InventoryUtils.shiftClickSlot(gui, slot.slotNumber);
                cancel = true;
            }

            this.draggedSlots.add(slot.slotNumber);
        }

        return cancel;
    }

    private boolean dragMoveFromSlotAtPositionCreative(GuiContainer gui, int x, int y, boolean leaveOneItem, boolean moveOnlyOne)
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
        boolean cancel = flag && (leaveOneItem || moveOnlyOne);
        // The player inventory tab of the creative inventory uses stupid wrapped
        // slots that all have slotNumber = 0 on the outer instance ;_;
        // However in that case we can use the slotIndex which is easy enough to get.
        int slotNumber = isPlayerInv ? AccessorUtils.getSlotIndex(slot) : slot.slotNumber;

        if (flag && slotNumber != this.slotNumberLast && this.draggedSlots.contains(slotNumber) == false)
        {
            if (moveOnlyOne)
            {
                this.leftClickSlot(guiCreative, slot, slotNumber);
                this.rightClickSlot(guiCreative, slot, slotNumber);
                this.shiftClickSlot(guiCreative, slot, slotNumber);
                this.leftClickSlot(guiCreative, slot, slotNumber);

                cancel = true;
            }
            else if (leaveOneItem)
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

            this.draggedSlots.add(slotNumber);
        }

        return cancel;
    }

    private void leftClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        AccessorUtils.handleMouseClick(gui, slot, slotNumber, 0, ClickType.PICKUP);
    }

    private void rightClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        AccessorUtils.handleMouseClick(gui, slot, slotNumber, 1, ClickType.PICKUP);
    }

    private void shiftClickSlot(GuiContainer gui, Slot slot, int slotNumber)
    {
        AccessorUtils.handleMouseClick(gui, slot, slotNumber, 0, ClickType.QUICK_MOVE);
    }
}
