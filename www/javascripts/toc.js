const tocDiv = document.getElementById('toc');
const subheaders = document.querySelectorAll('h2');

const list = document.createElement('ul');

subheaders.forEach((e, i) => {
    const tag = e.innerText.toLowerCase().split(' ').join('-');
    e.id = tag;

    const li = document.createElement('li');
    const a = document.createElement('a');

    a.href = `#${tag}`;
    a.textContent = e.innerText;

    li.appendChild(a);
    list.appendChild(li);
});

tocDiv.appendChild(list);
