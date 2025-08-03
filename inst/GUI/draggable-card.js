// 使所有可拖动卡片生效的函数
function initializeDraggableCards() {
  const cards = document.querySelectorAll('.draggable-card');

  cards.forEach(card => {
    const header = card.querySelector('.card-header');
    if (!header) return;

    header.addEventListener('mousedown', function(e) {
      e.preventDefault();

      let startX = e.clientX;
      let startY = e.clientY;
      let cardLeft = parseInt(window.getComputedStyle(card).left);
      let cardTop = parseInt(window.getComputedStyle(card).top);

      function moveCard(e) {
        const dx = e.clientX - startX;
        const dy = e.clientY - startY;
        // 移除bottom属性确保正常拖动
        card.style.bottom = '';
        card.style.left = (cardLeft + dx) + 'px';
        card.style.top = (cardTop + dy) + 'px';
      }

      function stopMoving() {
        document.removeEventListener('mousemove', moveCard);
        document.removeEventListener('mouseup', stopMoving);
      }

      document.addEventListener('mousemove', moveCard);
      document.addEventListener('mouseup', stopMoving);
    });
  });
}

// 当DOM加载完成后初始化
document.addEventListener('DOMContentLoaded', initializeDraggableCards);

// 为Shiny动态添加的卡片提供支持
if (window.Shiny) {
  Shiny.addCustomMessageHandler('initDraggableCard', function(data) {
    initializeDraggableCards(); // 重新初始化所有卡片
  });
}
