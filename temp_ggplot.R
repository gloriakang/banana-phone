library(ggplot2)
View (diamonds)

ggplot(data = diamonds) + geom_histogram(aes(x = price))
ggplot(data = diamonds) + geom_density(aes(x = price), fill = 'red')
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + geom_violin()

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + geom_boxplot() + facet_grid(color ~ clarity)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + geom_boxplot() + facet_wrap(~color)

ggplot(data = diamonds, mapping = aes(y = price, x = carat)) + geom_point()

ggplot(data = diamonds, mapping = aes(y = price, x = carat, color = color, size=price)) +
  geom_point(alpha=0.2)

ggplot(data = diamonds, mapping = aes(y = price, x = carat, color = color, size=price)) +
  geom_point(alpha=0.2) + facet_grid(cut ~ clarity)