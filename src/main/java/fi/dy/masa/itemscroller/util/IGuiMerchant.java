package fi.dy.masa.itemscroller.util;

import javax.annotation.Nullable;
import fi.dy.masa.itemscroller.gui.widgets.WidgetTradeList;

public interface IGuiMerchant
{
    int getSelectedMerchantRecipe();

    void setSelectedMerchantRecipe(int index);

    @Nullable
    WidgetTradeList getTradeListWidget();
}
