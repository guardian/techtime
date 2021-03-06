const getIsWhite = data => (x, y) => data[(x + y * 400) * 4] !== 0;

const isWhiteBox = (data, x, y, r) => {
  const isWhite = getIsWhite(data);
  const radiusSquared = r * r;
  for (let i = x - r; i <= x + r; i++) {
    for (let j = y - r; j <= y + r; j++) {
      const dx = i - x;
      const dy = j - y;
      const distanceSquared = dx * dx + dy * dy;
      if (!isWhite(i, j) && distanceSquared <= radiusSquared) {
        return false;
      }
    }
  }
  return true;
};

const findWhiteBoxCenter = ({ data, width, height }, w = 1) => {
  let r = 0;
  let pos;
  let x = r + w;
  while (x <= width - r - w) {
    let y = r + w;
    while (y <= height - r - w) {
      while (true) {
        if (!isWhiteBox(data, x, y, r)) {
          y++;
          break;
        }
        pos = { x, y };
        r++;
        x = x - r < 0 ? r : x;
        y = y - r < 0 ? r : y;
      }
    }
    x++;
  }
  return [pos, r - 1];
};

window.addEventListener('load', () => {
  const imgs = document.getElementsByTagName('img');

  [...imgs].forEach(img => {
    const canvas = document.createElement('canvas');

    canvas.width = 400;
    canvas.height = 400;

    const ctx = canvas.getContext('2d');

    ctx.drawImage(img, 0, 0);
    const data = ctx.getImageData(0, 0, 400, 400);

    const [{ x, y }, r] = findWhiteBoxCenter(data);
    ctx.arc(x, y, r, 0, Math.PI * 2);
    ctx.strokeStyle = 'red';
    ctx.stroke();

    img.src = canvas.toDataURL();
  });
});
